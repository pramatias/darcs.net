-- Copyright (C) 2002-2005,2007 David Roundy
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

module Darcs.Repository.ApplyPatches ( applyPatches, applyPatchesWithFeedback ) where

import Darcs.Patch ( Patchy, apply )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch.Info ( humanFriendly )
import Darcs.Witnesses.Ordered ( FL(..), lengthFL, mapFL )
import Progress ( beginTedious, endTedious, tediousSize, finishedOneIO )
import Printer ( hPutDocLn, text )
import Darcs.ColorPrinter () -- for instance Show Doc
import System.IO ( stderr )

applyPatchesWithFeedback :: Patchy p => String -> FL (PatchInfoAnd p) C(x y) -> IO ()
applyPatchesWithFeedback _ NilFL = return ()
applyPatchesWithFeedback k patches =
    do beginTedious k
       tediousSize k (lengthFL patches)
       sequence_ $ mapFL apply_cautiously patches
       endTedious k
    where apply_cautiously :: Patchy p => PatchInfoAnd p C(a b) -> IO ()
          apply_cautiously hp =
             do finishedOneIO k (show $ humanFriendly $ info hp)
                apply (hopefully hp) `catch` \e ->
                  do hPutDocLn stderr $ text "Unapplicable patch:"
                     hPutDocLn stderr $ humanFriendly (info hp)
                     ioError e

applyPatches :: Patchy p => FL (PatchInfoAnd p) C(x y) -> IO ()
applyPatches ps = applyPatchesWithFeedback "Applying patch" ps
