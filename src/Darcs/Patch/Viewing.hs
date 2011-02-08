-- Copyright (C) 2002-2004 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

module Darcs.Patch.Viewing
    ( showContextHunk, showContextSeries
    )
    where

import Prelude hiding ( pi, readFile )
import Control.Monad.State.Strict ( gets )
import Control.Monad.Trans ( liftIO )

import Storage.Hashed.Monad( TreeIO, fileExists, readFile, tree, virtualTreeIO )
import Storage.Hashed.AnchoredPath( floatPath )
import ByteStringUtils (linesPS )
import qualified Data.ByteString as BS (null, concat)
import qualified Data.ByteString.Lazy as BL (toChunks)
import Darcs.Patch.FileName ( fn2fp )
import Printer ( Doc, empty, vcat,
                 text, blueText, Color(Cyan,Magenta), lineColor,
                 ($$), (<+>),
                 prefix,
                 userchunkPS,
               )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(..), FileNameFormat(..) )
import Darcs.Patch.FileHunk ( IsHunk(..), FileHunk(..), showFileHunk )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), formatFileName )
import Darcs.Patch.Apply ( Apply(..), applyToTree )
#include "gadts.h"
import Darcs.Witnesses.Ordered ( RL(..), FL(..),
                             mapFL, mapFL_FL, reverseRL, concatFL )


showContextSeries :: (Apply p, ShowPatch p, IsHunk p) => FL p C(x y) -> TreeIO Doc
showContextSeries patches = scs Nothing patches
    where scs :: (Apply p, ShowPatch p, IsHunk p) => Maybe (FileHunk C(w x)) -> FL p C(x y) -> TreeIO Doc
          scs pold (p:>:ps) = do
            s' <- gets tree >>= liftIO . applyToTree p
            case isHunk p of
              Nothing -> do a <- showContextPatch p
                            b <- liftIO $ virtualTreeIO (scs Nothing ps) s'
                            return $ a $$ fst b
              Just fh ->
                  case ps of
                  NilFL -> coolContextHunk pold fh Nothing
                  (p2:>:_) -> do a <- coolContextHunk pold fh (isHunk p2)
                                 b <- liftIO $ virtualTreeIO (scs (Just fh) ps) s'
                                 return $ a $$ fst b
          scs _ NilFL = return empty

showContextHunk :: FileHunk C(x y) -> TreeIO Doc
showContextHunk h = coolContextHunk Nothing h Nothing

coolContextHunk :: Maybe (FileHunk C(a b)) -> FileHunk C(b c) -> Maybe (FileHunk C(c d)) -> TreeIO Doc
coolContextHunk prev fh@(FileHunk f l o n) next = do
  let path = floatPath $ fn2fp f
  have <- fileExists path
  content <- if have then Just `fmap` readFile path else return Nothing
  case (linesPS . BS.concat . BL.toChunks) `fmap` content of -- sigh
    Nothing -> return $ showFileHunk OldFormat fh -- This is a weird error...
    Just ls ->
        let numpre = case prev of
                     Just (FileHunk f' lprev _ nprev)
                         | f' == f &&
                           l - (lprev + length nprev + 3) < 3 &&
                           lprev < l ->
                             max 0 $ l - (lprev + length nprev + 3)
                     _ -> if l >= 4 then 3 else l - 1
            pre = take numpre $ drop (l - numpre - 1) ls
            numpost = case next of
                      Just (FileHunk f' lnext _ _)
                          | f' == f && lnext < l+length n+4 &&
                            lnext > l ->
                              lnext - (l+length n)
                      _ -> 3
            cleanedls = case reverse ls of
                        (x:xs) | BS.null x -> reverse xs
                        _ -> ls
            post = take numpost $ drop (max 0 $ l+length o-1) cleanedls
            in return $ blueText "hunk" <+> formatFileName OldFormat f <+> text (show l)
            $$ prefix " " (vcat $ map userchunkPS pre)
            $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS o))
            $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS n))
            $$ prefix " " (vcat $ map userchunkPS post)

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (FL p) where
    showPatch = showPatchInternal patchListFormat
      where showPatchInternal :: ListFormat p -> FL p C(x y) -> Doc
            showPatchInternal ListFormatV1      (p :>: NilFL) = showPatch p
            showPatchInternal ListFormatV1      NilFL         = blueText "{" $$ blueText "}"
            showPatchInternal ListFormatV1      ps            = blueText "{" $$ vcat (mapFL showPatch ps) $$ blueText "}"
            showPatchInternal ListFormatV2      ps            = vcat (mapFL showPatch ps)
            showPatchInternal ListFormatDefault ps            = vcat (mapFL showPatch ps)

instance (Apply p, IsHunk p, PatchListFormat p, ShowPatch p) => ShowPatch (FL p) where
    showContextPatch = showContextPatchInternal patchListFormat
      where showContextPatchInternal :: ListFormat p -> FL p C(x y) -> TreeIO Doc
            showContextPatchInternal ListFormatV1      (p :>: NilFL) = showContextPatch p
            showContextPatchInternal ListFormatV1      NilFL         = return $ blueText "{" $$ blueText "}"
            showContextPatchInternal ListFormatV1      ps            = do x <- showContextSeries ps
                                                                          return $ blueText "{" $$ x $$ blueText "}"
            showContextPatchInternal ListFormatV2      ps            = showContextSeries ps
            showContextPatchInternal ListFormatDefault ps            = showContextSeries ps

    description = vcat . mapFL description
    summary = summaryFL
    summaryFL = summaryFL . concatFL
    thing x = thing (helperx x) ++ "s"
        where helperx :: FL a C(x y) -> a C(x y)
              helperx _ = undefined
    things = thing

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (RL p) where
    showPatch = showPatch . reverseRL

instance (Apply p, IsHunk p, PatchListFormat p, ShowPatch p) => ShowPatch (RL p) where
    showContextPatch = showContextPatch . reverseRL
    description = description . reverseRL
    summary = summary . reverseRL
    summaryFL = summaryFL . mapFL_FL reverseRL
    thing = thing . reverseRL
    things = things . reverseRL


