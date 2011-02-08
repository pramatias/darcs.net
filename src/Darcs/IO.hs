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

{-# LANGUAGE TypeSynonymInstances, UndecidableInstances #-}
module Darcs.IO ( ReadableDirectory(..), WriteableDirectory(..),
                  TolerantIO, runTolerantly, runSilently,
                ) where

import Prelude hiding ( catch )
import Data.Char ( toLower )
import Data.List ( isSuffixOf )
import System.IO.Error ( isDoesNotExistError, isPermissionError )
import Control.Exception.Extensible ( catch, SomeException, IOException )
import Control.Monad.Error
import System.Directory ( getDirectoryContents, createDirectory,
                          removeDirectory, removeFile,
                          renameFile, renameDirectory,
                          doesDirectoryExist, doesFileExist,
                        )

import ByteStringUtils ( linesPS, unlinesPS)
import qualified Data.ByteString as B (ByteString, empty, null, readFile, concat)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Darcs.Utils ( withCurrentDirectory, prettyException )
import Darcs.External ( backupByCopying, backupByRenaming )
import Printer ( Doc, renderPS )
import Darcs.Patch.FileName ( FileName, fn2fp, fp2fn )
import Darcs.Lock ( writeBinFile, readBinFile, writeAtomicFilePS )
import Workaround ( setExecutable )

import qualified Storage.Hashed.Monad as HSM
import Storage.Hashed.AnchoredPath( AnchoredPath, floatPath )

class (Functor m, MonadPlus m) => ReadableDirectory m where
    mDoesDirectoryExist :: FileName -> m Bool
    mDoesFileExist :: FileName -> m Bool
    mInCurrentDirectory :: FileName -> m a -> m a
    mGetDirectoryContents :: m [FileName]
    mReadBinFile :: FileName -> m String
    mReadBinFile f = liftM BC.unpack $ mReadFilePS f
    mReadFilePS :: FileName -> m B.ByteString
    mReadFilePSs :: FileName -> m [B.ByteString]
    mReadFilePSs f = linesPS `liftM` mReadFilePS f

class ReadableDirectory m => WriteableDirectory m where
    mWithCurrentDirectory :: FileName -> m a -> m a
    mSetFileExecutable :: FileName -> Bool -> m ()
    mWriteBinFile :: FileName -> String -> m ()
    mWriteBinFile fn s = mWriteFilePS fn $ BC.pack s
    mWriteFilePS :: FileName -> B.ByteString -> m ()
    mWriteFilePSs :: FileName -> [B.ByteString] -> m ()
    mWriteFilePSs f ss = mWriteFilePS f (unlinesPS ss)
    mCreateDirectory :: FileName -> m ()
    mRemoveDirectory :: FileName -> m ()
    mWriteDoc :: FileName -> Doc -> m ()
    mWriteDoc f d = mWriteFilePS f (renderPS d)
    mCreateFile :: FileName -> m ()
    mCreateFile f = mWriteFilePS f B.empty
    mRemoveFile :: FileName -> m ()
    mRename :: FileName -> FileName -> m ()
    mModifyFilePS :: FileName -> (B.ByteString -> m B.ByteString) -> m ()
    mModifyFilePS f j = do ps <- mReadFilePS f
                           ps' <- j ps
                           mWriteFilePS f ps'
    mModifyFilePSs :: FileName -> ([B.ByteString] -> m [B.ByteString]) -> m ()
    mModifyFilePSs f j = do ps <- mReadFilePSs f
                            ps' <- j ps
                            mWriteFilePSs f ps'

instance ReadableDirectory IO where
    mDoesDirectoryExist = doesDirectoryExist . fn2fp
    mDoesFileExist = doesFileExist . fn2fp
    mInCurrentDirectory = withCurrentDirectory . fn2fp
    mGetDirectoryContents = map fp2fn `liftM` getDirectoryContents "."
    mReadBinFile = readBinFile . fn2fp
    mReadFilePS = B.readFile . fn2fp

instance WriteableDirectory IO where
    mWithCurrentDirectory = mInCurrentDirectory
    mSetFileExecutable = setExecutable . fn2fp
    mWriteBinFile = writeBinFile . fn2fp
    mWriteFilePS = writeAtomicFilePS . fn2fp
    mCreateDirectory = createDirectory . fn2fp
    mCreateFile f = do exf <- mDoesFileExist f
                       if exf then fail $ "File '"++fn2fp f++"' already exists!"
                              else do exd <- mDoesDirectoryExist f
                                      if exd then fail $ "File '"++fn2fp f++"' already exists!"
                                             else mWriteFilePS f B.empty
    mRemoveFile f = do let fp = fn2fp f
                       x <- B.readFile fp
                       when (not $ B.null x) $
                            fail $ "Cannot remove non-empty file "++fp
                       removeFile fp
    mRemoveDirectory = removeDirectory . fn2fp
    mRename a b = catch
                  (renameDirectory x y `mplus` renameFile x y)
                  -- We need to catch does not exist errors, since older
                  -- versions of darcs allowed users to rename nonexistent
                  -- files.  :(
                  (\e -> if isDoesNotExistError e
                                 then return ()
                                 else ioError e)
      where x = fn2fp a
            y = fn2fp b

class Monad m => TolerantMonad m where
    warning :: IO () -> m ()
    runIO :: m a -> IO a
    runTM :: IO a -> m a

newtype TolerantIO a = TIO { runTolerantly :: IO a }
instance TolerantMonad TolerantIO where
    warning io = TIO $ io `catch` \e -> putStrLn $ "Warning: " ++ prettyException e
    runIO (TIO io) = io
    runTM io = TIO io

newtype SilentIO a = SIO { runSilently :: IO a }
instance TolerantMonad SilentIO where
    warning io = SIO $ io `catch` \(_ :: SomeException) -> return ()
    runIO (SIO io) = io
    runTM io = SIO io

-- NOTE: The following instance declarations are duplicated merely to avoid
-- enabling -fallow-undecidable-instances.  If we used
-- -fallow-undecidable-instances, we would write instead:

-- instance TolerantMonad m => Monad m where
--      ...

-- etc.
instance Functor TolerantIO where
    fmap f m = m >>= return . f

instance Monad TolerantIO where
    f >>= g = runTM $ runIO f >>= runIO . g
    f >> g = runTM $ runIO f >> runIO g
    fail s = runTM $ fail s
    return x = runTM $ return x

instance Functor SilentIO where
    fmap f m = m >>= return . f

instance Monad SilentIO where
    f >>= g = runTM $ runIO f >>= runIO . g
    f >> g = runTM $ runIO f >> runIO g
    fail s = runTM $ fail s
    return x = runTM $ return x

instance MonadPlus TolerantIO where
    mzero = runTM mzero
    mplus a b = runTM (mplus (runIO a) (runIO b))
instance MonadPlus SilentIO where
    mzero = runTM mzero
    mplus a b = runTM (mplus (runIO a) (runIO b))

instance ReadableDirectory TolerantIO where
    mDoesDirectoryExist d = runTM $ mDoesDirectoryExist d
    mDoesFileExist f = runTM $ mDoesFileExist f
    mInCurrentDirectory i j = runTM $ mInCurrentDirectory i (runIO j)
    mGetDirectoryContents = runTM mGetDirectoryContents
    mReadBinFile f = runTM $ mReadBinFile f
    mReadFilePS f = runTM $ mReadFilePS f
instance ReadableDirectory SilentIO where
    mDoesDirectoryExist d = runTM $ mDoesDirectoryExist d
    mDoesFileExist f = runTM $ mDoesFileExist f
    mInCurrentDirectory i j = runTM $ mInCurrentDirectory i (runIO j)
    mGetDirectoryContents = runTM mGetDirectoryContents
    mReadBinFile f = runTM $ mReadBinFile f
    mReadFilePS f = runTM $ mReadFilePS f

instance WriteableDirectory TolerantIO where
     mWithCurrentDirectory = mInCurrentDirectory
     mSetFileExecutable f e = warning $ mSetFileExecutable f e
     mWriteBinFile f s = warning $ mWriteBinFile f s
     mWriteFilePS f s = warning $ mWriteFilePS f s
     mCreateFile f = warning $ backup f >> mWriteFilePS f B.empty
     mCreateDirectory d = warning $ backup d >> mCreateDirectory d
     mRemoveFile f = warning $ mRemoveFile f
     mRemoveDirectory d = warning $ catch
                                 (mRemoveDirectory d)
                                 (\(e :: IOException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because:\n" ++ show e)
     mRename a b = warning $ catch
                          (let do_backup = if (map toLower x == map toLower y)
                                           then backupByCopying y -- avoid making the original vanish
                                           else backupByRenaming y
                           in do_backup >> mRename a b)
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y
instance WriteableDirectory SilentIO where
     mWithCurrentDirectory = mInCurrentDirectory
     mSetFileExecutable f e = warning $ mSetFileExecutable f e
     mWriteBinFile f s = warning $ mWriteBinFile f s
     mWriteFilePS f s = warning $ mWriteFilePS f s
     mCreateFile f = warning $ backup f >> mWriteFilePS f B.empty
     mCreateDirectory d = warning $ backup d >> mCreateDirectory d
     mRemoveFile f = warning $ mRemoveFile f
     mRemoveDirectory d = warning $ catch
                                 (mRemoveDirectory d)
                                 (\(e :: SomeException) ->
                                   if "(Directory not empty)" `isSuffixOf` show e
                                   then ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because it is not empty."
                                   else ioError $ userError $
                                            "Not deleting " ++ fn2fp d ++ " because:\n" ++ show e)
     mRename a b = warning $ catch
                          (let do_backup = if (map toLower x == map toLower y)
                                           then backupByCopying y -- avoid making the original vanish
                                           else backupByRenaming y
                           in do_backup >> mRename a b)
                          (\e -> case () of
                                 _ | isPermissionError e -> ioError $ userError $
                                       couldNotRename ++ "."
                                   | isDoesNotExistError e -> ioError $ userError $
                                       couldNotRename ++ " because " ++ x ++ " does not exist."
                                   | otherwise -> ioError e
                          )
       where
        x = fn2fp a
        y = fn2fp b
        couldNotRename = "Could not rename " ++ x ++ " to " ++ y

backup :: FileName -> IO ()
backup f = backupByRenaming $ fn2fp f

floatFn :: FileName -> AnchoredPath
floatFn = floatPath . fn2fp

instance (Functor m, MonadPlus m, MonadError e m) => ReadableDirectory (HSM.TreeMonad m) where
    mDoesDirectoryExist d = HSM.directoryExists (floatFn d)
    mDoesFileExist f = HSM.fileExists (floatFn f)
    mInCurrentDirectory d action = HSM.withDirectory (floatFn d) action
    mGetDirectoryContents = error "get dir contents"
    mReadFilePS p = do x <- HSM.readFile (floatFn p)
                       return $ B.concat (BL.toChunks x)

instance (Functor m, MonadPlus m, MonadError e m) => WriteableDirectory (HSM.TreeMonad m) where
    mWithCurrentDirectory = mInCurrentDirectory
    mSetFileExecutable _ _ = return ()
    mWriteFilePS p ps = HSM.writeFile (floatFn p) (BL.fromChunks [ps])
    mCreateDirectory p = HSM.createDirectory (floatFn p)
    mRename from to = HSM.rename (floatFn from) (floatFn to)
    mRemoveDirectory = HSM.unlink . floatFn
    mRemoveFile = HSM.unlink . floatFn
