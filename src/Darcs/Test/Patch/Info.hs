-- Copyright (C) 2009 Reinier Lamers
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

-- | This module contains tests for the code in Darcs.Patch.Info. Most of them
--   are about the UTF-8-encoding of patch metadata.
module Darcs.Test.Patch.Info ( testSuite ) where

import Prelude hiding ( pi )
import Data.ByteString ( ByteString )
import qualified Data.ByteString as B ( split, pack )
import qualified Data.ByteString.Char8 as BC ( unpack )
import Data.List ( sort )
import Data.Maybe ( isNothing )
import Data.Text as T ( find, any )
import Data.Text.Encoding ( decodeUtf8With )
import Data.Text.Encoding.Error ( lenientDecode )
import Foreign ( unsafePerformIO )
import Test.QuickCheck ( Arbitrary(arbitrary), oneof, listOf, choose, shrink
                       , Gen )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework (Test, testGroup)
import Data.List ( isPrefixOf )

import Darcs.Patch.Info ( PatchInfo(..), patchinfo,
                          piLog, piAuthor, piName )
import ByteStringUtils ( decodeLocale, packStringToUTF8, unpackPSFromUTF8 )

testSuite :: Test
testSuite = testGroup "Darcs.Patch.Info"
  [ metadataDecodingTest
  , metadataEncodingTest
  , packUnpackTest
  ]

-- | A newtype wrapping String so we can make our own random generator for it.
newtype UnicodeString = UnicodeString { asString :: String }
        deriving (Show, Eq, Ord)

-- | A newtype wrapping PatchInfo that has a random generator that generates
--   both UTF-8-encoded and non-encoded PatchInfo's.
newtype UTF8OrNotPatchInfo = UTF8OrNotPatchInfo PatchInfo deriving (Eq, Ord)

-- | A newtype wrapping PatchInfo, which has a random generator that generates
--   only UTF-8-encoded PatchInfo's.
newtype UTF8PatchInfo = UTF8PatchInfo PatchInfo deriving (Eq, Ord)

instance Arbitrary UnicodeString where
    -- 0x10ffff is the highest Unicode code point ; 0xd800 - 0xdfff are
    -- surrogates. '\xfffd' is excluded because it is used as a marker
    -- for UTF-8 test failure.
    arbitrary = UnicodeString `fmap` listOf (oneof [choose ('\0', '\xd799')
                                                   ,choose ('\xe000', '\xfffc')
                                                   ,choose ('\xfffe', '\x10ffff')])

instance Show UTF8PatchInfo where
     showsPrec _ = withUTF8PatchInfo rawPatchInfoShow
instance Show UTF8OrNotPatchInfo where
     showsPrec _ = withUTF8OrNotPatchInfo rawPatchInfoShow

-- | Shows a PatchInfo, outputting every byte and clearly marking what is what
rawPatchInfoShow :: PatchInfo -> String -> String
rawPatchInfoShow pi = ("PatchInfo: \n"++)
                    . ("date: "++) . shows (_piDate pi) . ('\n':)
                    . ("author: "++) . shows (_piAuthor pi) . ('\n':)
                    . ("name: "++) . shows (_piName pi) . ('\n':)
                    . ("log: "++) . shows (_piLog pi) . ('\n':)

instance Arbitrary UTF8PatchInfo where
    arbitrary = UTF8PatchInfo `fmap` arbitraryUTF8Patch
    shrink upi = flip withUTF8PatchInfo upi $ \pi -> do
        sn <- shrink (piName pi)
        sa <- shrink (piAuthor pi)
        sl <- shrink (filter (not . isPrefixOf "Ignore-this:") (piLog pi))
        return (UTF8PatchInfo
                   (unsafePerformIO $ patchinfo sn
                                          (BC.unpack (_piDate pi)) sa sl))

instance Arbitrary UTF8OrNotPatchInfo where
    arbitrary = UTF8OrNotPatchInfo `fmap` oneof ([arbitraryUTF8Patch,
                                                  arbitraryUnencodedPatch])

-- | Generate arbitrary patch metadata that uses the metadata creation function
--   'patchinfo' from Darcs.Patch.Info.
arbitraryUTF8Patch :: Gen PatchInfo
arbitraryUTF8Patch =
    do n <- asString `fmap` arbitrary
       d <- arbitrary
       a <- asString `fmap` arbitrary
       l <- (lines . asString) `fmap` arbitrary
       return $ unsafePerformIO $ patchinfo n d a l

-- | Generate arbitrary patch metadata that has totally arbitrary byte strings
--   as its name, date, author and log.
arbitraryUnencodedPatch :: Gen PatchInfo
arbitraryUnencodedPatch = do
    n <- arbitraryByteString
    d <- arbitraryByteString
    a <- arbitraryByteString
    -- split 10 is the ByteString equivalent of 'lines'
    l <- B.split 10 `fmap` arbitraryByteString
    i <- arbitrary
    return (PatchInfo d n a l i)

arbitraryByteString :: Gen ByteString
arbitraryByteString = (B.pack . map fromIntegral)
                          `fmap` listOf (choose (0, 255) :: Gen Int)

-- | Test that anything produced by the 'patchinfo' function is valid UTF-8
metadataEncodingTest :: Test
metadataEncodingTest = testProperty "Testing patch metadata encoding" $
    withUTF8PatchInfo $
       \patchInfo -> encodingOK (_piAuthor patchInfo)
                     && encodingOK (_piName patchInfo)
                     && all encodingOK (_piLog patchInfo)
  where encodingOK = isNothing . T.find (=='\xfffd') . decodeUtf8With lenientDecode

-- | Test that metadata in patches are decoded as UTF-8 or locale depending on
-- whether they're valid UTF-8.
metadataDecodingTest :: Test
metadataDecodingTest = testProperty "Testing patch metadata decoding" $
    withUTF8OrNotPatchInfo $
        \patchInfo -> utf8OrLocale (_piAuthor patchInfo) == piAuthor patchInfo
                      && utf8OrLocale (_piName patchInfo) == piName patchInfo
                      && map utf8OrLocale (_piLog patchInfo) `superset` piLog patchInfo
  where utf8OrLocale bs = if isValidUTF8 bs
                            then unpackPSFromUTF8 bs
                            else decodeLocale bs

isValidUTF8 :: ByteString -> Bool
isValidUTF8 = not . T.any (=='\xfffd') . decodeUtf8With lenientDecode

packUnpackTest :: Test
packUnpackTest = testProperty "Testing UTF-8 packing and unpacking" $
    \uString -> asString uString == (unpackPSFromUTF8 . packStringToUTF8) (asString uString)

superset :: (Eq a, Ord a) => [a] -> [a] -> Bool
superset a b = sorted_superset (sort a) (sort b)
  where sorted_superset (x:xs) (y:ys) | x == y = sorted_superset xs ys
                                      | x <  y = sorted_superset xs (y:ys)
                                      | otherwise = False
        sorted_superset []     (_:_)           = False
        sorted_superset _      []              = True

withUTF8PatchInfo :: (PatchInfo -> a) -> UTF8PatchInfo -> a
withUTF8PatchInfo f mpi = case mpi of
                            UTF8PatchInfo pinf -> f pinf
withUTF8OrNotPatchInfo :: (PatchInfo -> a) -> UTF8OrNotPatchInfo -> a
withUTF8OrNotPatchInfo f mpi = case mpi of
                                 UTF8OrNotPatchInfo pinf -> f pinf
