-- Copyright (C) 2002-2003 David Roundy
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

-- | FileName is an abstract type intended to facilitate the input and output of
-- unicode filenames.
module Darcs.Patch.FileName ( FileName( ),
                              fp2fn, fn2fp,
                              fn2ps, ps2fn,
                              niceps2fn, fn2niceps,
                              breakOnDir, normPath, ownName, superName,
                              movedirfilename,
                              encodeWhite, decodeWhite,
                              (///),
                              breakup
                            ) where

import Data.Char ( isSpace, chr, ord )
import ByteStringUtils ( packStringToUTF8, unpackPSFromUTF8 )
import qualified Data.ByteString.Char8 as BC (unpack, pack)
import qualified Data.ByteString       as B  (ByteString)

newtype FileName = FN FilePath deriving ( Eq, Ord )

instance Show FileName where
   showsPrec d (FN fp) = showParen (d > appPrec) $ showString "fp2fn " . showsPrec (appPrec + 1) fp
      where appPrec = 10

{-# INLINE fp2fn #-}
fp2fn :: FilePath -> FileName
fp2fn fp = FN fp

{-# INLINE fn2fp #-}
fn2fp :: FileName -> FilePath
fn2fp (FN fp) = fp

{-# INLINE niceps2fn #-}
niceps2fn :: B.ByteString -> FileName
niceps2fn = FN . decodeWhite . BC.unpack

{-# INLINE fn2niceps #-}
fn2niceps :: FileName -> B.ByteString
fn2niceps (FN fp) = BC.pack $ encodeWhite fp

{-# INLINE fn2ps #-}
fn2ps :: FileName -> B.ByteString
fn2ps (FN fp) = packStringToUTF8 $ encodeWhite fp

{-# INLINE ps2fn #-}
ps2fn :: B.ByteString -> FileName
ps2fn ps = FN $ decodeWhite $ unpackPSFromUTF8 ps

-- | 'encodeWhite' translates whitespace in filenames to a darcs-specific
--   format (numerical representation according to 'ord' surrounded by
--   backslashes).  Note that backslashes are also escaped since they are used
--   in the encoding.
--
--   > encodeWhite "hello there" == "hello\32\there"
--   > encodeWhite "hello\there" == "hello\92\there"
encodeWhite :: FilePath -> String
encodeWhite (c:cs) | isSpace c || c == '\\' =
    '\\' : (show $ ord c) ++ "\\" ++ encodeWhite cs
encodeWhite (c:cs) = c : encodeWhite cs
encodeWhite [] = []

-- | 'decodeWhite' interprets the Darcs-specific \"encoded\" filenames
--   produced by 'encodeWhite'
--
--   > decodeWhite "hello\32\there"  == "hello there"
--   > decodeWhite "hello\92\there"  == "hello\there"
--   > decodeWhite "hello\there"   == error "malformed filename"
decodeWhite :: String -> FilePath
decodeWhite ('\\':cs) =
    case break (=='\\') cs of
    (theord, '\\':rest) ->
        chr (read theord) : decodeWhite rest
    _ -> error "malformed filename"
decodeWhite (c:cs) = c: decodeWhite cs
decodeWhite "" = ""

ownName :: FileName -> FileName
ownName (FN f) =  case breakLast '/' f of Nothing -> FN f
                                          Just (_,f') -> FN f'
superName :: FileName -> FileName
superName fn = case normPath fn of
                FN f -> case breakLast '/' f of
                        Nothing -> FN "."
                        Just (d,_) -> FN d
breakOnDir :: FileName -> Maybe (FileName,FileName)
breakOnDir (FN p) = case breakFirst '/' p of
                      Nothing -> Nothing
                      Just (d,f) | d == "." -> breakOnDir $ FN f
                                 | otherwise -> Just (FN d, FN f)
normPath :: FileName -> FileName -- remove "./"
normPath (FN p) = FN $ repath $ dropDotdot $ breakup p

repath :: [String] -> String
repath [] = ""
repath [f] = f
repath (d:p) = d ++ "/" ++ repath p

dropDotdot :: [String] -> [String]
dropDotdot ("":p) = dropDotdot p
dropDotdot (".":p) = dropDotdot p
dropDotdot ("..":p) = ".." : (dropDotdot p)
dropDotdot (_:"..":p) = dropDotdot p
dropDotdot (d:p) = case dropDotdot p of
                    ("..":p') -> p'
                    p' -> d : p'
dropDotdot [] = []

-- | Split a file path at the slashes
breakup :: String -> [String]
breakup p = case break (=='/') p of
            (d,"") -> [d]
            (d,p') -> d : breakup (tail p')

breakFirst :: Char -> String -> Maybe (String,String)
breakFirst c l = bf [] l
    where bf a (r:rs) | r == c = Just (reverse a,rs)
                      | otherwise = bf (r:a) rs
          bf _ [] = Nothing
breakLast :: Char -> String -> Maybe (String,String)
breakLast c l = case breakFirst c (reverse l) of
                Nothing -> Nothing
                Just (a,b) -> Just (reverse b, reverse a)

(///) :: FileName -> FileName -> FileName
(FN "")///b = normPath b
a///b = normPath $ fp2fn $ fn2fp a ++ "/" ++ fn2fp b

movedirfilename :: FileName -> FileName -> FileName -> FileName
movedirfilename old new name =
    if name' == old' then new
                     else if length name' > length old' &&
                             take (length old'+1) name' == old'++"/"
                          then fp2fn ("./"++new'++drop (length old') name')
                          else name
    where old' = fn2fp $ normPath old
          new' = fn2fp $ normPath new
          name' = fn2fp $ normPath name
