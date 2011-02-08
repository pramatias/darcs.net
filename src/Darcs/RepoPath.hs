{-# LANGUAGE CPP #-}

-- Copyright (C) 2007 Eric Kow
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

-- | Various abstractions for dealing with paths.
module Darcs.RepoPath (
  -- * AbsolutePath
  AbsolutePath,
  makeAbsolute,
  ioAbsolute,
  rootDirectory,
  -- * AbsolutePathOrStd
  AbsolutePathOrStd,
  makeAbsoluteOrStd,
  ioAbsoluteOrStd,
  useAbsoluteOrStd,
  stdOut,
  -- * AbsoluteOrRemotePath
  AbsoluteOrRemotePath,
  ioAbsoluteOrRemote,
  isRemote,
  -- * SubPath
  SubPath,
  makeSubPathOf,
  simpleSubPath,
  -- * Miscellaneous
  sp2fn,
  FilePathOrURL(..),
  FilePathLike(toFilePath),
  getCurrentDirectory,
  setCurrentDirectory
) where

import Data.List ( isPrefixOf, isSuffixOf )
import Control.Exception ( bracket )

import Darcs.URL ( isAbsolute, isRelative, isSshNopath )
import qualified Workaround ( getCurrentDirectory )
import qualified System.Directory ( setCurrentDirectory )
import System.Directory ( doesDirectoryExist )
import qualified System.FilePath.Posix as FilePath ( normalise )
import qualified System.FilePath as NativeFilePath ( takeFileName, takeDirectory )
import qualified Darcs.Patch.FileName as PatchFileName ( FileName, fp2fn, fn2fp )
#include "impossible.h"

class FilePathOrURL a where
 {-# INLINE toPath #-}
 toPath :: a -> String

class FilePathOrURL a => FilePathLike a where
 {-# INLINE toFilePath #-}
 toFilePath :: a -> FilePath

-- | Paths which are relative to the local darcs repository and normalized.
-- Note: These are understood not to have the dot in front.
newtype SubPath      = SubPath FilePath deriving (Eq, Ord)

newtype AbsolutePath = AbsolutePath FilePath deriving (Eq, Ord)

-- | This is for situations where a string (e.g. a command line argument)
-- may take the value \"-\" to mean stdin or stdout (which one depends on
-- context) instead of a normal file path.
data AbsolutePathOrStd = AP AbsolutePath | APStd deriving (Eq, Ord)
data AbsoluteOrRemotePath = AbsP AbsolutePath | RmtP String deriving (Eq, Ord)

instance FilePathOrURL AbsolutePath where
 toPath (AbsolutePath x) = x
instance FilePathOrURL SubPath where
 toPath (SubPath x) = x
instance CharLike c => FilePathOrURL [c] where
 toPath = toFilePath

instance FilePathOrURL AbsoluteOrRemotePath where
 toPath (AbsP a) = toPath a
 toPath (RmtP r) = r

instance FilePathOrURL PatchFileName.FileName where
    toPath = PatchFileName.fn2fp
instance FilePathLike PatchFileName.FileName where
    toFilePath = PatchFileName.fn2fp

instance FilePathLike AbsolutePath where
 toFilePath (AbsolutePath x) = x
instance FilePathLike SubPath where
 toFilePath (SubPath x) = x

class CharLike c where
    toChar :: c -> Char
    fromChar :: Char -> c
instance CharLike Char where
    toChar = id
    fromChar = id

instance CharLike c => FilePathLike [c] where
    toFilePath = map toChar

-- | Make the second path relative to the first, if possible
makeSubPathOf :: AbsolutePath -> AbsolutePath -> Maybe SubPath
makeSubPathOf (AbsolutePath p1) (AbsolutePath p2) =
 -- The slash prevents "foobar" from being treated as relative to "foo"
 if p1 == p2 || (p1 ++ "/") `isPrefixOf` p2
    then Just $ SubPath $ drop (length p1 + 1) p2
    else Nothing

simpleSubPath :: FilePath -> Maybe SubPath
simpleSubPath x | null x = bug "simpleSubPath called with empty path"
                | isRelative x = Just $ SubPath $ FilePath.normalise $ pathToPosix x
                | otherwise = Nothing

-- | Interpret a possibly relative path wrt the current working directory.
ioAbsolute :: FilePath -> IO AbsolutePath
ioAbsolute dir =
    do isdir <- doesDirectoryExist dir
       here <- getCurrentDirectory
       if isdir
         then bracket (setCurrentDirectory dir)
                      (const $ setCurrentDirectory $ toFilePath here)
                      (const getCurrentDirectory)
         else let super_dir = case NativeFilePath.takeDirectory dir of
                                "" ->  "."
                                d  -> d
                  file = NativeFilePath.takeFileName dir
              in do abs_dir <- if dir == super_dir
                               then return $ AbsolutePath dir
                               else ioAbsolute super_dir
                    return $ makeAbsolute abs_dir file

-- | Take an absolute path and a string representing a (possibly relative)
-- path and combine them into an absolute path. If the second argument is
-- already absolute, then the first argument gets ignored. This function also
-- takes care that the result is converted to Posix convention and
-- normalized. Also, parent directories (\"..\") at the front of the string
-- argument get canceled out against trailing directory parts of the
-- absolute path argument.
--
-- Regarding the last point, someone more familiar with how these functions
-- are used should verify that this is indeed necessary or at least useful.
makeAbsolute :: AbsolutePath -> FilePath -> AbsolutePath
makeAbsolute a dir = if not (null dir) && isAbsolute dir
                     then AbsolutePath (normSlashes dir')
                     else ma a dir'
  where
    dir' = FilePath.normalise $ pathToPosix dir
    -- Why do we care to reduce ".." here?
    -- Why not do this throughout the whole path, i.e. "x/y/../z" -> "x/z" ?
    ma here ('.':'.':'/':r) = ma (takeDirectory here) r
    ma here ".." = takeDirectory here
    ma here "." = here
    ma here "" = here
    ma here r = here /- ('/':r)

(/-) :: AbsolutePath -> String -> AbsolutePath
x /- ('/':r) = x /- r
(AbsolutePath "/") /- r = AbsolutePath ('/':simpleClean r)
(AbsolutePath x) /- r = AbsolutePath (x++'/':simpleClean r)

-- | Convert to posix, remove trailing slashes, and (under Posix)
-- reduce multiple leading slashes to one.
simpleClean :: String -> String
simpleClean = normSlashes . reverse . dropWhile (=='/') . reverse . pathToPosix

-- | The root directory as an absolute path.
rootDirectory :: AbsolutePath
rootDirectory = AbsolutePath "/"

makeAbsoluteOrStd :: AbsolutePath -> String -> AbsolutePathOrStd
makeAbsoluteOrStd _ "-" = APStd
makeAbsoluteOrStd a p = AP $ makeAbsolute a p

stdOut :: AbsolutePathOrStd
stdOut = APStd

ioAbsoluteOrStd :: String -> IO AbsolutePathOrStd
ioAbsoluteOrStd "-" = return APStd
ioAbsoluteOrStd p = AP `fmap` ioAbsolute p

-- | Execute either the first or the second argument action, depending on
-- whether the given path is an 'AbsolutePath' or stdin/stdout.
useAbsoluteOrStd :: (AbsolutePath -> a) -> a -> AbsolutePathOrStd -> a
useAbsoluteOrStd _ f APStd = f
useAbsoluteOrStd f _ (AP x) = f x

ioAbsoluteOrRemote :: String -> IO AbsoluteOrRemotePath
ioAbsoluteOrRemote p = do
  isdir <- doesDirectoryExist p
  if not isdir
     then return $ RmtP $
          case () of _ | isSshNopath p    -> p++"."
                       | "/" `isSuffixOf` p -> init p
                       | otherwise          -> p
     else AbsP `fmap` ioAbsolute p

isRemote :: AbsoluteOrRemotePath -> Bool
isRemote (RmtP _) = True
isRemote _ = False

takeDirectory :: AbsolutePath -> AbsolutePath
takeDirectory (AbsolutePath x) =
    case reverse $ drop 1 $ dropWhile (/='/') $ reverse x of
    "" -> AbsolutePath "/"
    x' -> AbsolutePath x'

instance Show AbsolutePath where
 show = show . toFilePath
instance Show SubPath where
 show = show . toFilePath
instance Show AbsolutePathOrStd where
    show (AP a) = show a
    show APStd = "standard input/output"
instance Show AbsoluteOrRemotePath where
    show (AbsP a) = show a
    show (RmtP r) = show r

-- | Normalize the path separator to Posix style (slash, not backslash).
-- This only affects Windows systems.
pathToPosix :: FilePath -> FilePath
pathToPosix = map convert where
#ifdef WIN32
  convert '\\' = '/'
#endif
  convert c = c

-- | Reduce multiple leading slashes to one. This only affects Posix systems.
normSlashes :: FilePath -> FilePath
#ifndef WIN32
-- multiple slashes in front are ignored under Posix
normSlashes ('/':p) = '/' : dropWhile (== '/') p
#endif
normSlashes p = p

getCurrentDirectory :: IO AbsolutePath
getCurrentDirectory = AbsolutePath `fmap` Workaround.getCurrentDirectory

setCurrentDirectory :: FilePathLike p => p -> IO ()
setCurrentDirectory = System.Directory.setCurrentDirectory . toFilePath

{-# INLINE sp2fn #-}
sp2fn :: SubPath -> PatchFileName.FileName
sp2fn = PatchFileName.fp2fn . toFilePath
