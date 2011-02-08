-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

module Darcs.Repository.LowLevel
    ( readPending, readTentativePending
    , writeTentativePending
    -- deprecated interface:
    , readNewPending, writeNewPending
    , pendingName )
    where

import Darcs.Repository.InternalTypes ( RepoType(..), Repository(..) )
import Darcs.Patch ( readPatch, writePatch, RepoPatch, PrimOf )
import Darcs.Patch.Read ( ReadPatch(..), bracketedFL )
import Darcs.Patch.ReadMonads ( ParserM )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Global ( darcsdir )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), mapSeal )
import Darcs.Witnesses.Ordered ( FL(..), mapFL )
import Darcs.Utils ( catchall )
import ByteStringUtils ( gzReadFilePS )
import Printer ( Doc, ($$), text, vcat )

import Control.Applicative
import qualified Data.ByteString as BS ( ByteString, empty )

pendingName :: RepoType p -> String
pendingName (DarcsRepository _ _) = darcsdir++"/patches/pending"

-- | Read the contents of pending. CWD should be the repository directory.
-- The return type is currently incorrect as it refers to the tentative
-- state rather than the recorded state.
readPending :: RepoPatch p => Repository p C(r u t) -> IO (Sealed (FL (PrimOf p) C(t)))
readPending (Repo _ _ _ tp) =
    readPendingFile (pendingName tp)

-- |Read the contents of tentative pending. CWD should be the repository directory.
readTentativePending :: RepoPatch p => Repository p C(r u t) -> IO (Sealed (FL (PrimOf p) C(t)))
readTentativePending (Repo _ _ _ tp) =
    readPendingFile (pendingName tp ++ ".tentative")

-- |Read the contents of tentative pending. CWD should be the repository directory.
readNewPending :: RepoPatch p => Repository p C(r u t) -> IO (Sealed (FL (PrimOf p) C(t)))
readNewPending (Repo _ _ _ tp) =
    readPendingFile (pendingName tp ++ ".new")

readPendingFile :: ReadPatch prim => String -> IO (Sealed (FL prim C(x)))
readPendingFile name = do
  pend <- gzReadFilePS name `catchall` return BS.empty
  return $ readPendingContents pend

-- Wrapper around FL where printed format uses { } except around singletons
-- Now that the Show behaviour of FL p can be customised (using showFLBehavior),
-- we could instead change the general behaviour of FL Prim; but since the pending
-- code can be kept nicely compartmentalised, it's nicer to do it this way.
newtype FLM p C(x y) = FLM { unFLM :: FL p C(x y) }

instance ReadPatch p => ReadPatch (FLM p) where
   readPatch' = mapSeal FLM <$> readMaybeBracketedFL readPatch' '{' '}'

instance ShowPatchBasic p => ShowPatchBasic (FLM p) where
   showPatch = showMaybeBracketedFL showPatch '{' '}' . unFLM

readPendingContents :: ReadPatch prim => BS.ByteString -> Sealed (FL prim C(x))
readPendingContents = maybe (Sealed NilFL) (mapSeal unFLM) . readPatch

writePendingFile :: ShowPatchBasic prim => String -> FL prim C(x y) -> IO ()
writePendingFile name = writePatch name . FLM

readMaybeBracketedFL :: forall m p C(x) . ParserM m =>
                         (FORALL(y) m (Sealed (p C(y)))) -> Char -> Char -> m (Sealed (FL p C(x)))
readMaybeBracketedFL parser pre post =
  bracketedFL parser pre post <|> (mapSeal (:>:NilFL) <$> parser)

showMaybeBracketedFL :: (FORALL(x y) p C(x y) -> Doc) -> Char -> Char -> FL p C(a b) -> Doc
showMaybeBracketedFL _       pre post NilFL         = text [pre] $$ text [post]
showMaybeBracketedFL printer _   _    (p :>: NilFL) = printer p
showMaybeBracketedFL printer pre post ps            = text [pre] $$ vcat (mapFL printer ps) $$ text [post]

-- |Read the contents of tentative pending. CWD should be the repository directory.
writeTentativePending :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(t y) -> IO ()
writeTentativePending (Repo _ _ _ tp) pend =
    writePendingFile (pendingName tp ++ ".tentative") pend

-- |Read the contents of new pending. CWD should be the repository directory.
writeNewPending :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(t y) -> IO ()
writeNewPending (Repo _ _ _ tp) pend =
    writePendingFile (pendingName tp ++ ".new") pend