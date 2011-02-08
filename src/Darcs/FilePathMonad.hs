-- Copyright (C) 2005 David Roundy
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

module Darcs.FilePathMonad ( FilePathMonad, withFilePaths ) where

import Control.Monad ( MonadPlus, mplus, mzero )
import Data.Maybe ( mapMaybe )

import Darcs.IO ( ReadableDirectory(..), WriteableDirectory(..) )
import Darcs.Patch.FileName ( FileName, fp2fn, fn2fp, superName, breakOnDir,
                              normPath, movedirfilename )
#include "impossible.h"

data FilePathMonad a = FPM ([FileName] -> ([FileName], a))

withFilePaths :: [FilePath] -> FilePathMonad a -> [FilePath]
withFilePaths fps (FPM x) = map fn2fp $ fst $ x $ map fp2fn fps

instance Functor FilePathMonad where
    fmap f m = m >>= return . f

instance Monad FilePathMonad where
    (FPM x) >>= y = FPM z where z fs = case x fs of
                                       (fs', a) -> case y a of
                                                   FPM yf -> yf fs'
    return x = FPM $ \fs -> (fs, x)

instance MonadPlus FilePathMonad where
    mzero = fail "mzero FilePathMonad" -- yuck!
    a `mplus` _ = a

instance ReadableDirectory FilePathMonad where
    -- We can't check it actually is a directory here
    mDoesDirectoryExist d =
        FPM $ \fs -> (fs, normPath d `elem` map normPath fs)
    -- We can't check it actually is a file here
    mDoesFileExist f =
        FPM $ \fs -> (fs, normPath f `elem` map normPath fs)
    mInCurrentDirectory d (FPM j) =
        FPM $ \fs -> (fs, snd $ j $ mapMaybe indir fs)
        where indir f = do (d',f') <- breakOnDir f
                           if d == d' then Just f'
                                      else Nothing
    mGetDirectoryContents =
        FPM $ \fs -> (fs, filter (\f -> fp2fn "." == superName f) fs)
    mReadFilePS = bug "can't mReadFilePS in FilePathMonad!"

instance WriteableDirectory FilePathMonad where
    mWithCurrentDirectory d (FPM j) =
        FPM $ \fs ->
        let splitfs = map splitf fs
            others = mapMaybe snd splitfs
            (myfs, a) = j $ mapMaybe fst splitfs
            splitf f = case breakOnDir f of
                       Just (d', f') | d' == d -> (Just f', Nothing)
                       _ -> (Nothing, Just f)
        in (others ++ myfs, a)
    mSetFileExecutable _ _ = return ()
    mWriteFilePS _ _ = return ()
    mCreateDirectory _ = return ()
    mRemoveFile f = FPM $ \fs -> (filter (/= f) fs, ())
    mRemoveDirectory f = FPM $ \fs -> (filter (/= f) fs, ())
    mRename a b = FPM $ \fs -> (map (movedirfilename a b) fs, ())
    mModifyFilePS _ _ = return ()
    mModifyFilePSs _ _ = return ()
