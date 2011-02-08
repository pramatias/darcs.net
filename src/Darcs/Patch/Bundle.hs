-- Copyright (C) 2002-2004,2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.Bundle ( hashBundle, makeBundle2, makeBundleN, scanBundle,
                            contextPatches, scanContext, patchFilename, getContext,
                            parseBundle
                   ) where

import Data.Char ( isAlpha, toLower, isDigit, isSpace )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, piap, fmapFL_PIAP,
                         patchInfoAndPatch,
                         unavailable, hopefully )
import Darcs.Patch ( RepoPatch, Named, showPatch, showContextPatch, readPatchPartial )
import Darcs.Patch.Bracketed ( Bracketed, unBracketedFL )
import Darcs.Patch.Bracketed.Instances ()
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo, humanFriendly, isTag )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Depends ( slightlyOptimizePatchset )
import Darcs.Patch.ReadMonads ( parseStrictly )
import Darcs.Patch.PatchInfoAnd( info )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( RL(..), FL(..), (:>)(..),
                             reverseFL, (+<+), mapFL, mapFL_FL, mapRL )
import Printer ( Doc, renderPS, newline, text, ($$),
                 (<>), vcat, vsep, renderString )

import ByteStringUtils ( linesPS, unlinesPS, dropSpace, substrPS)
import qualified Data.ByteString as B (ByteString, length, null, drop, isPrefixOf)
import qualified Data.ByteString.Char8 as BC (unpack, break, pack)

import SHA1( sha1PS )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), mapSeal )

import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeIO )

hashBundle :: (PatchListFormat p, ShowPatchBasic p) => [PatchInfo] -> FL (Named p) C(x y) -> String
hashBundle _ to_be_sent = sha1PS $ renderPS
                         $ vcat (mapFL showPatch to_be_sent) <> newline

makeBundleN :: RepoPatch p => Maybe (Tree IO)
             -> PatchSet p C(start x) -> FL (Named p) C(x y) -> IO Doc
makeBundleN the_s (PatchSet ps (Tagged t _ _ :<: _)) to_be_sent =
    makeBundle2 the_s (ps +<+ (t :<: NilRL)) to_be_sent to_be_sent
makeBundleN the_s (PatchSet ps NilRL) to_be_sent =
    makeBundle2 the_s ps to_be_sent to_be_sent

-- | In makeBundle2, it is presumed that the two patch sequences are
-- identical, but that they may be lazily generated.  If two different
-- patch sequences are passed, a bundle with a mismatched hash will be
-- generated, which is not the end of the world, but isn't very useful
-- either.
makeBundle2 :: RepoPatch p => Maybe (Tree IO) -> RL (PatchInfoAnd p) C(start x)
             -> FL (Named p) C(x y) -> FL (Named p) C(x y) -> IO Doc
makeBundle2 the_s common' to_be_sent to_be_sent2 =
    do patches <- case the_s of
                    Just tree -> fst `fmap` virtualTreeIO (showContextPatch to_be_sent) tree
                    Nothing -> return (vsep $ mapFL showPatch to_be_sent)
       return $ format patches
    where format the_new = text ""
                           $$ text "New patches:"
                           $$ text ""
                           $$ the_new
                           $$ text ""
                           $$ text "Context:"
                           $$ text ""
                           $$ (vcat $ map showPatchInfo common)
                           $$ text "Patch bundle hash:"
                           $$ text (hashBundle common to_be_sent2)
                           $$ text ""
          common = mapRL info common'

parseBundle ::forall p. RepoPatch p => B.ByteString
              -> Either String (Sealed ((PatchSet p :> FL (PatchInfoAnd p)) C(Origin)))
parseBundle ps
  | B.null ps = Left "Bad patch bundle!"
  | otherwise =
    case sillyLex ps of
    ("New patches:",rest) ->
        case getPatches rest of
        (Sealed patchesBraced, rest') ->
            let patches = mapFL_FL (fmapFL_PIAP unBracketedFL) patchesBraced in
            case sillyLex rest' of
            ("Context:", rest'') ->
                case getContext rest'' of
                (cont,maybe_hash) ->
                    case substrPS (BC.pack "Patch bundle hash:")
                         maybe_hash of
                    Just n ->
                        if hashBundle cont (mapFL_FL hopefully patchesBraced)
                               == fst (sillyLex $ snd $ sillyLex $
                                       B.drop n maybe_hash)
                        then Right $ seal_up_patches patches cont
                        else Left $
                                 "Patch bundle failed hash!\n" ++
                                 "This probably means that the patch has been "++
                                 "corrupted by a mailer.\n"++
                                 "The most likely culprit is CRLF newlines."
                    Nothing -> Right $ seal_up_patches patches cont
            (a,r) -> Left $ "Malformed patch bundle: '"++a++"' is not 'Context:'"
                     ++ "\n" ++ BC.unpack r
    ("Context:",rest) ->
        case getContext rest of
        (cont, rest') ->
            case sillyLex rest' of
            ("New patches:", rest'') ->
                case parsePatches rest'' of
                Sealed ps'' -> Right $ seal_up_patches ps'' cont
            (a,_) -> Left $ "Malformed patch bundle: '" ++ a ++
                     "' is not 'New patches:'"
    ("-----BEGIN PGP SIGNED MESSAGE-----",rest) ->
            parseBundle $ filterGpgDashes rest
    (_,rest) -> parseBundle rest
    where seal_up_patches :: RepoPatch p => FL (PatchInfoAnd p) C(x y) -> [PatchInfo]
                             -> Sealed ((PatchSet p :> FL (PatchInfoAnd p)) C(Origin))
          seal_up_patches patches context =
              case reverse context of
              (x:ry) | isTag x ->
                        Sealed $ (PatchSet
                                  (unavailablePatches (reverse ry))
                                  (Tagged (piUnavailable x) Nothing NilRL :<: NilRL)
                                  :> patches)
              _ -> Sealed ((PatchSet (unavailablePatches context)
                            NilRL) :> patches)
                   -- The above NilRLs aren't quite right, because ther *are*
                   -- earlier patches, but we can't set this to undefined
                   -- because there are situations where we look at the rest.
                   -- :{


scanBundle ::forall p . RepoPatch p => B.ByteString
             -> Either String (SealedPatchSet p C(Origin))
scanBundle bundle = do
  Sealed ((PatchSet recent tagged):>ps) <- parseBundle bundle
  return . Sealed $ PatchSet (reverseFL ps +<+ recent) tagged

-- filterGpgDashes is needed because clearsigned patches escape dashes:
filterGpgDashes :: B.ByteString -> B.ByteString
filterGpgDashes ps =
    unlinesPS $ map drop_dashes $
    takeWhile (/= BC.pack "-----END PGP SIGNED MESSAGE-----") $
    dropWhile not_context_or_newpatches $ linesPS ps
    where drop_dashes x = if B.length x < 2 then x
                          else if BC.pack "- " `B.isPrefixOf` x
                               then B.drop 2 x
                               else x
          not_context_or_newpatches s = (s /= BC.pack "Context:") &&
                                        (s /= BC.pack "New patches:")

unavailablePatches :: RepoPatch p => [PatchInfo] -> RL (PatchInfoAnd p) C(x y)
unavailablePatches [] = unsafeCoerceP NilRL
unavailablePatches (x:xs) = piUnavailable x :<: unavailablePatches xs

piUnavailable :: RepoPatch p => PatchInfo -> PatchInfoAnd p C(x y)
piUnavailable i = (i `patchInfoAndPatch`
                      unavailable ("Patch not stored in patch bundle:\n" ++
                                   renderString (humanFriendly i)))
getContext :: B.ByteString -> ([PatchInfo],B.ByteString)
getContext ps =
    case parseStrictly readPatchInfo ps of
    Just (pinfo,r') ->
        case getContext r' of
        (pis,r'') -> (pinfo:pis, r'')
    Nothing -> ([],ps)
(-:-) :: a C(x y) -> (Sealed (FL a C(y)),b) -> (Sealed (FL a C(x)),b)
p -:- (Sealed ps, r) = (Sealed (p:>:ps), r)
getPatches :: RepoPatch p => B.ByteString -> (Sealed (FL (PatchInfoAnd (Bracketed p)) C(x)), B.ByteString)
getPatches ps =
    case parseStrictly readPatchInfo ps of
    Nothing -> (Sealed NilFL, ps)
    Just (pinfo,_) ->
        case readPatchPartial ps of
        Nothing -> (Sealed NilFL, ps)
        Just (Sealed p, r) -> (pinfo `piap` p) -:- getPatches r
parsePatches :: RepoPatch p => B.ByteString -> Sealed (FL (PatchInfoAnd p) C(x))
parsePatches ps =
  case parseStrictly readPatchInfo ps of
  Nothing -> Sealed NilFL
  Just (pinfo,_) ->
    case readPatchPartial ps of
    Nothing -> Sealed NilFL
    Just (Sealed p, r) -> ((pinfo `piap` p) :>:) `mapSeal` parsePatches r

sillyLex :: B.ByteString -> (String, B.ByteString)
sillyLex ps = (BC.unpack a, b)
    where
        (a, b) = BC.break (== '\n') (dropSpace ps)

{-
sillyLex ps = (BC.unpack $ BC.takeWhile (/='\n') ps', BC.dropWhile (/='\n') ps')
    where
        ps' = dropSpace ps
-}


contextPatches :: RepoPatch p => PatchSet p C(Origin x) ->
                  (PatchSet p :> (RL (PatchInfoAnd p))) C(Origin x)
contextPatches set = case slightlyOptimizePatchset set of
  PatchSet ps (Tagged t _ ps' :<: ts)
      -> (PatchSet ps' ts) :> (ps +<+ (t :<: NilRL))
  PatchSet ps NilRL -> (PatchSet NilRL NilRL :> ps)

-- are the type witnesses sensible?
scanContext :: RepoPatch p => B.ByteString -> PatchSet p C(Origin x)
scanContext ps
  | B.null ps = error "Bad context!"
  | otherwise =
    case sillyLex ps of
    ("Context:",rest) ->
        case getContext rest of
          (cont@(_:_), _) | isTag (last cont) ->
            PatchSet (unavailablePatches $ init cont)
                     (Tagged (piUnavailable $ last cont) Nothing NilRL :<: NilRL)
          (cont, _) -> PatchSet (unavailablePatches cont) NilRL
    ("-----BEGIN PGP SIGNED MESSAGE-----",rest) ->
            scanContext $ filterGpgDashes rest
    (_,rest) -> scanContext rest


patchFilename :: String -> String
patchFilename the_summary = name ++ ".dpatch"
    where name = map safeFileChar the_summary
          safeFileChar c | isAlpha c = toLower c
                         | isDigit c = c
                         | isSpace c = '-'
          safeFileChar _ = '_'
