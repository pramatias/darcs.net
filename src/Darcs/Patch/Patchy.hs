-- Copyright (C) 2007 David Roundy
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

module Darcs.Patch.Patchy
     ( Patchy,
       Apply(..), Commute(..), Invert(..),
       PatchInspect(..), ReadPatch(..),
       showPatch, ShowPatch(..)
     ) where

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Commute ( Commute(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Read ( ReadPatch(..) )
import Darcs.Patch.Show ( showPatch, ShowPatch(..), ShowPatchBasic )
import Darcs.Witnesses.Eq ( MyEq )

class (MyEq p, Apply p, Commute p, PatchInspect p, ShowPatch p, ReadPatch p, Invert p) => Patchy p

