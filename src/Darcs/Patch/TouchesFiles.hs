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

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.TouchesFiles ( lookTouch, chooseTouching, choosePreTouching,
                      selectTouching,
                      deselectNotTouching, selectNotTouching,
                    ) where
import Data.List ( sort, isSuffixOf )

import Darcs.Patch.Choices ( PatchChoices, Tag, TaggedPatch,
                             patchChoices, tag, getChoices,
                      forceFirsts, forceLasts, tpPatch,
                    )
import Darcs.Patch ( Patchy, applyToFilepaths, listTouchedFiles, invert )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), mapFL_FL, (+>+) )
import Darcs.Witnesses.Sealed ( Sealed, seal )

selectTouching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
selectTouching [] pc = pc
selectTouching files pc = forceFirsts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouch fs (tpPatch tp) of
                             (True, fs') -> tag tp:ct fs' tps
                             (False, fs') -> ct fs' tps
          xs = case getChoices pc of
               _ :> mc :> lc -> ct (map fix files) (mc +>+ lc)

deselectNotTouching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
deselectNotTouching [] pc = pc
deselectNotTouching files pc = forceLasts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouch fs (tpPatch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case getChoices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

selectNotTouching :: Patchy p => [FilePath] -> PatchChoices p C(x y) -> PatchChoices p C(x y)
selectNotTouching [] pc = pc
selectNotTouching files pc = forceFirsts xs pc
    where ct :: Patchy p => [FilePath] -> FL (TaggedPatch p) C(x y) -> [Tag]
          ct _ NilFL = []
          ct fs (tp:>:tps) = case lookTouch fs (tpPatch tp) of
                             (True, fs') -> ct fs' tps
                             (False, fs') -> tag tp:ct fs' tps
          xs = case getChoices pc of
               fc :> mc :> _ -> ct (map fix files) (fc +>+ mc)

fix :: FilePath -> FilePath
fix f | "/" `isSuffixOf` f = fix $ init f
fix "" = "."
fix "." = "."
fix f = "./" ++ f

chooseTouching :: Patchy p => [FilePath] -> FL p C(x y) -> Sealed (FL p C(x))
chooseTouching [] p = seal p
chooseTouching files p = case getChoices $ selectTouching files $ patchChoices p of
                          fc :> _ :> _ -> seal $ mapFL_FL tpPatch fc

choosePreTouching :: (Patchy p) => [FilePath] -> FL p C(x y) -> Sealed (FL p C(x))
choosePreTouching files patch = do
  let pre_files = applyToFilepaths (invert patch) files
      relevant = case files of
                   [] -> seal patch
                   _ -> chooseTouching pre_files patch
   in relevant

lookTouch :: Patchy p => [FilePath] -> p C(x y) -> (Bool, [FilePath])
lookTouch fs p = (any (\tf -> any (affects tf) fs) (listTouchedFiles p)
                   || fs' /= fs, fs')
    where affects :: FilePath -> FilePath -> Bool
          affects touched f =  touched == f
                            || touched `isSubPathOf` f
                            || f `isSubPathOf` touched
          isSubPathOf :: FilePath -> FilePath -> Bool
          isSubPathOf sub path = case splitAt (length sub) path of
                                 (path', '/':_) -> path' == sub
                                 _ -> False
          fs' = sort $ applyToFilepaths p fs
