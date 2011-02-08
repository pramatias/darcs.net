-- Copyright (C) 2003 David Roundy
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

module Darcs.PrintPatch ( printPatch, contextualPrintPatch,
                          printPatchPager, printFriendly ) where

import Darcs.Patch ( Patchy, showContextPatch, showPatch )
import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeIO )
import Darcs.Arguments ( DarcsFlag, showFriendly )
import Printer ( putDocLnWith )
import Darcs.ColorPrinter ( fancyPrinters )
import Darcs.External ( viewDocWith )

-- | @'printFriendly' opts patch@ prints @patch@ in accordance with the
-- flags in opts, ie, whether @--verbose@ or @--summary@ were passed at
-- the command-line.
printFriendly :: Patchy p => [DarcsFlag] -> p C(x y) -> IO ()
printFriendly opts p = putDocLnWith fancyPrinters $ showFriendly opts p

-- | 'printPatch' prints a patch on standard output.
printPatch :: Patchy p => p C(x y) -> IO ()
printPatch p = putDocLnWith fancyPrinters $ showPatch p

-- | 'printPatchPager' runs '$PAGER' and shows a patch in it.
printPatchPager :: Patchy p => p C(x y) -> IO ()
printPatchPager p = viewDocWith fancyPrinters $ showPatch p

-- | 'contextualPrintPatch' prints a patch, together with its context,
-- on standard output.
contextualPrintPatch :: Patchy p => Tree IO -> p C(x y) -> IO ()
contextualPrintPatch s p = virtualTreeIO (showContextPatch p) s >>= putDocLnWith fancyPrinters . fst
