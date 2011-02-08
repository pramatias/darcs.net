--  Copyright (C) 2002-2005,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

module Darcs.Test.Patch ( testInfo, testSuite ) where

import Darcs.Test.Patch.Examples ( testInfo, patchExampleTests )
import Darcs.Test.Patch.Properties2 ( patchPropertyTests )
import Darcs.Test.Patch.Unit ( patchUnitTests )
import Darcs.Test.Patch.Unit2 ( patchUnitTests2 )
import qualified Darcs.Test.Patch.Info
import Test.Framework ( Test, testGroup )

-- | This is the big list of tests that will be run using testrunner.
testSuite :: Test
testSuite = testGroup "Darcs.Patch" $
            patchUnitTests ++ patchUnitTests2 ++ patchPropertyTests ++
            patchExampleTests ++ [Darcs.Test.Patch.Info.testSuite]
