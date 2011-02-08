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

{-# LANGUAGE CPP #-}

module Darcs.Test.Misc ( testSuite ) where

import ByteStringUtils ( unpackPSFromUTF8, fromHex2PS, fromPS2Hex )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )
import qualified Data.ByteString as B ( concat, empty )
import Lcs ( shiftBoundaries )
import Data.Array.Base
import Control.Monad.ST
import Test.HUnit ( assertBool, assertEqual, assertFailure )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test, testGroup )

#include "gadts.h"

testSuite :: Test
testSuite = testGroup ""
 [ byteStringUtilsTestSuite
 , lcsTestSuite
 ]


-- ----------------------------------------------------------------------
-- * ByteStringUtils
-- Here are a few quick tests of the shiftBoundaries function.
-- ----------------------------------------------------------------------

byteStringUtilsTestSuite :: Test
byteStringUtilsTestSuite = testGroup "ByteStringUtils"
  [ testCase "UTF-8 packing and unpacking preserves 'hello world'"
           (assertBool "" (unpackPSFromUTF8 (BC.pack "hello world") == "hello world"))
  , testCase "Checking that hex packing and unpacking preserves 'hello world'"
           (assertEqual "" (BC.unpack (fromHex2PS $ fromPS2Hex $ BC.pack "hello world"))
                           "hello world")
  , testProperty "Checking that B.concat works" propConcatPS
  , testProperty "Checking that hex conversion works" propHexConversion
  ]

propHexConversion :: String -> Bool
propHexConversion s =
    fromHex2PS (fromPS2Hex $ BC.pack s) == BC.pack s

propConcatPS :: [String] -> Bool
propConcatPS ss = concat ss == BC.unpack (B.concat $ map BC.pack ss)

-- ----------------------------------------------------------------------
-- * LCS
-- Here are a few quick tests of the shiftBoundaries function.
-- ----------------------------------------------------------------------

lcsTestSuite :: Test
lcsTestSuite = testGroup "LCS"
 [ testCase "lcs code" (mapM_ assertFailure showLcsTests)
 ]

showLcsTests :: [String]
showLcsTests = concatMap checkKnownShifts knownShifts
checkKnownShifts :: ([Int],[Int],String,String,[Int],[Int])
                   -> [String]
checkKnownShifts (ca, cb, sa, sb, ca', cb') = runST (
    do ca_arr <- newListArray (0, length ca) $ toBool (0:ca)
       cb_arr <- newListArray (0, length cb) $ toBool (0:cb)
       let p_a = listArray (0, length sa) $ B.empty:(toPS sa)
           p_b = listArray (0, length sb) $ B.empty:(toPS sb)
       shiftBoundaries ca_arr cb_arr p_a 1 1
       shiftBoundaries cb_arr ca_arr p_b 1 1
       ca_res <- fmap (fromBool . tail) $ getElems ca_arr
       cb_res <- fmap (fromBool . tail) $ getElems cb_arr
       return $ if ca_res  == ca' && cb_res == cb' then []
                else ["shiftBoundaries failed on "++sa++" and "++sb++" with "
                      ++(show (ca,cb))++" expected "++(show (ca', cb'))
                      ++" got "++(show (ca_res, cb_res))++"\n"])
 where toPS = map (\c -> if c == ' ' then B.empty else BC.pack [c])
       toBool = map (>0)
       fromBool = map (\b -> if b then 1 else 0)

knownShifts :: [([Int],[Int],String,String,[Int],[Int])]
knownShifts =
  [([0,0,0],[0,1,0,1,0],"aaa","aaaaa",
    [0,0,0],[0,0,0,1,1]),
   ([0,1,0],[0,1,1,0],"cd ","c a ",
    [0,1,0],[0,1,1,0]),
   ([1,0,0,0,0,0,0,0,0],[1,0,0,0,0,0,1,1,1,1,1,0,0,0], "fg{} if{}","dg{} ih{} if{}",
    [1,0,0,0,0,0,0,0,0],[1,0,0,0,0,1,1,1,1,1,0,0,0,0]), -- prefer empty line at end
   ([0,0,0,0,0,0,0,0,0],[0,0,0,0,0,0,1,1,1,1,1,0,0,0], "fg{} if{}","fg{} ih{} if{}",
    [0,0,0,0,0,0,0,0,0],[0,0,0,0,0,1,1,1,1,1,0,0,0,0]), -- prefer empty line at end
   ([],[1,1],"","aa",[],[1,1]),
   ([1,1],[],"aa","",[1,1],[])]
