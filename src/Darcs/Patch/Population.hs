{-# LANGUAGE CPP #-}

--   Copyright (C) 2003-2004 Jan Scheffczyk and David Roundy
--
--   This program is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2, or (at your option)
--   any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program; see the file COPYING.  If not, write to
--   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--   Boston, MA 02110-1301, USA.

#include "gadts.h"

module Darcs.Patch.Population ( Population, applyToPop,
                    getPopFrom,
                    setPopState,
                    DirMark(..),
                    getRepoPopVersion,
                    modifiedToXml,
                    lookupPop, lookupCreationPop,
                  ) where

import qualified Data.ByteString.Char8 as BC ( unpack, singleton, pack )
import Data.Maybe ( mapMaybe )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.FileName ( fn2fp, fp2fn, fn2ps, normPath )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.Named ( patchcontents )
import Darcs.Patch.Prim ( PrimPatch, PrimPatchBase, applyToPopPrim )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), dropWhileRL )
import Darcs.Patch.Info ( PatchInfo, idpatchinfo, toXml )
import Darcs.Patch.Set ( PatchSet(..), newset2FL, newset2RL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Sealed ( Sealed(..), seal, unseal )
import Darcs.Repository ( withRepositoryDirectory, RepoJob(..), readRepo )
import Darcs.Patch.PopulationData ( Population(..), PopTree(..), Info(..), DirMark(..),
                        setPopState, getPopFrom )
import Printer ( empty, text, ($$), (<>), Doc )
import Control.Monad ( liftM )

#include "impossible.h"

-- | population of an empty repository
initPop :: Population PatchInfo
initPop = Pop idpatchinfo (PopDir i [])
 where i = Info {nameI          = BC.singleton '.',
                 modifiedByI    = idpatchinfo,
                 modifiedHowI   = DullDir,
                 createdByI     = Nothing,
                 creationNameI  = Just (BC.singleton '.')}

-- | apply a patchset to a population
applyPatchSetPop :: (PrimPatchBase p, Effect p) => PatchSet p C(Origin x) -> Population PatchInfo -> Population PatchInfo
applyPatchSetPop ps pop = applyPatchesPop (newset2FL ps) pop

-- | apply Patches to a population
applyPatchesPop :: (PrimPatchBase p, Effect p) => FL (PatchInfoAnd p) C(x y) -> Population PatchInfo -> Population PatchInfo
applyPatchesPop NilFL = id
applyPatchesPop (hp:>:hps) = applyPatchesPop hps .
                             applyToPop (info hp) (effect $ patchcontents $ hopefully hp)

applyToPop :: (Eq pi, PrimPatch prim) => pi -> FL prim C(x y) -> Population pi -> Population pi
applyToPop _ NilFL = id
applyToPop pinf (p:>:ps) = applyToPop pinf ps . applyToPopPrim pinf p

getRepoPopVersion :: FilePath -> PatchInfo -> IO (Population PatchInfo)
getRepoPopVersion repobasedir pinfo = withRepositoryDirectory [] repobasedir $ RepoJob $ \repository ->
   do pips <- newset2RL `liftM` readRepo repository
      return $ (unseal applyPatchSetPop) (mkPatchSet $ dropWhileRL ((/=pinfo).info) pips) initPop
             where mkPatchSet (Sealed xs) = seal $ PatchSet xs NilRL

-- Routines for pulling data conveniently out of a Population

lookupPop :: FilePath -> Population pi -> Maybe (Population pi)
lookupPop f p = lookupPop' (BC.unpack $ fn2ps $ fp2fn f) p

lookupPop' :: String -> Population pi -> Maybe (Population pi)
lookupPop' f p@(Pop _ (PopFile i))
    | BC.unpack (nameI i) == f = Just p
    | otherwise = Nothing
lookupPop' d p@(Pop pinfo (PopDir i c))
    | BC.unpack (nameI i) == "." =
        case mapMaybe (lookupPop' (dropDS d).(Pop pinfo)) c of
        [apop] -> Just apop
        [] -> Nothing
        _ -> impossible
    | BC.unpack (nameI i) == takeWhile (/='/') d =
        case dropWhile (=='/') $ dropWhile (/='/') d of
        "" -> Just p
        d' -> case mapMaybe (lookupPop' d'.(Pop pinfo)) c of
              [apop] -> Just apop
              [] -> Nothing
              _ -> impossible
    | otherwise = Nothing
    where dropDS ('.':'/':f) = dropDS f
          dropDS f = f

lookupCreationPop :: Eq pi => pi -> FilePath -> Population pi -> Maybe (Population pi)
lookupCreationPop pinfo f p = lookupCreationPop' pinfo (BC.unpack $ fn2ps $ fp2fn f) p

lookupCreationPop' :: Eq pi => pi -> String -> Population pi -> Maybe (Population pi)
lookupCreationPop' b a (Pop pinfo pp) = (Pop pinfo) `fmap` lcp pp
    where lcp p@(PopFile i)
              | fixname `fmap` creationNameI i == f && createdByI i == who = Just p
              | otherwise = Nothing
          lcp p@(PopDir i c)
              | fixname `fmap` creationNameI i == f && createdByI i == who = Just p
              | otherwise = case mapMaybe lcp c of
                            [apop] -> Just apop
                            _ -> Nothing
          fixname = BC.pack . fn2fp . normPath . fp2fn . BC.unpack
          f = Just $ BC.pack $ fn2fp $ normPath $ fp2fn a
          who = Just b

modifiedToXml :: Info PatchInfo -> Doc
modifiedToXml i | modifiedHowI i == DullDir = empty
                  | modifiedHowI i == DullFile = empty
modifiedToXml i = text "<modified>"
                 $$ text "<modified_how>" <> text (show (modifiedHowI i)) <>
                    text "</modified_how>"
                 $$ toXml (modifiedByI i)
                 $$ text "</modified>"
