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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP, TypeSynonymInstances, FlexibleInstances #-}

#include "gadts.h"

module Darcs.Repository.HashedIO ( HashedIO,
                                   copyHashed, copyPartialsHashed,
                                   cleanHashdir,
                                   RW(RW) -- only exported to make warning go away
                                 ) where

import Darcs.Global ( darcsdir )
import qualified Data.Set as Set
import System.Directory ( getDirectoryContents, createDirectoryIfMissing )
import Control.Monad.State ( StateT, runStateT, modify, get, put, gets, lift )
import Control.Monad ( when )
import Control.Applicative ( (<$>) )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafeInterleaveIO )

import Darcs.Repository.Cache ( Cache(..), fetchFileUsingCache, writeFileUsingCache,
                                peekInCache, speculateFileUsingCache,
                                okayHash, cleanCachesWithHint, HashedDir(..), hashedDir )
import Darcs.RepoPath ( FilePathLike, toFilePath )
import Darcs.IO ( ReadableDirectory(..), WriteableDirectory(..) )
import Darcs.Flags ( Compression( .. ) )
import Darcs.Lock ( writeAtomicFilePS, removeFileMayNotExist )
import Darcs.Utils ( withCurrentDirectory )
import Progress ( debugMessage, tediousSize, finishedOneIO )
import Darcs.Patch.FileName ( FileName, normPath, fp2fn, fn2fp, fn2niceps, niceps2fn,
                              breakOnDir, ownName, superName )

import ByteStringUtils ( linesPS, unlinesPS )
import qualified Data.ByteString       as B  (ByteString, length, empty)
import qualified Data.ByteString.Char8 as BC (unpack, pack)

import Storage.Hashed.Darcs( readDarcsHashedDir, darcsLocation,
                             decodeDarcsHash, decodeDarcsSize )
import Storage.Hashed.Tree( ItemType(..) )

-- | @readHashFile c subdir hash@ reads the file with hash @hash@ in dir subdir,
-- fetching it from 'Cache' @c@ if needed.
readHashFile :: Cache -> HashedDir -> String -> IO (String,B.ByteString)
readHashFile c subdir hash =
    do debugMessage $ "Reading hash file "++hash++" from "++(hashedDir subdir)++"/"
       fetchFileUsingCache c subdir hash

data HashDir r p = HashDir { permissions :: !r, cache :: !Cache,
                             compress :: !Compression, rootHash :: !String }
type HashedIO r p = StateT (HashDir r p) IO

data RO = RO
data RW = RW
{-
class Readable r where
    isRO :: r -> Bool
    isRO = const False
instance Readable RW
instance Readable RO where
    isRO RO = True
-}

instance ReadableDirectory (HashedIO r p) where
    mDoesDirectoryExist fn = do thing <- identifyThing fn
                                case thing of Just (D,_) -> return True
                                              _ -> return False
    mDoesFileExist fn = do thing <- identifyThing fn
                           case thing of Just (F,_) -> return True
                                         _ -> return False
    mInCurrentDirectory fn j | fn' == fp2fn "" = j
                             | otherwise =
                                 case breakOnDir fn' of
                                 Nothing -> do c <- readroot
                                               case geta D fn' c of
                                                 Nothing -> fail "dir doesn't exist mInCurrentDirectory..."
                                                 Just h -> inh h j
                                 Just (d,fn'') -> do c <- readroot
                                                     case geta D d c of
                                                       Nothing -> fail "dir doesn't exist..."
                                                       Just h -> inh h $ mInCurrentDirectory fn'' j
        where fn' = normPath fn
    mGetDirectoryContents = map (\ (_,f,_) -> f) `fmap` readroot
    mReadFilePS fn = mInCurrentDirectory (superName fn) $ do
                                          c <- readroot
                                          case geta F (ownName fn) c of
                                            Nothing -> fail $ " file don't exist... "++ fn2fp fn
                                            Just h -> readhash h

instance WriteableDirectory (HashedIO RW p) where
    mWithCurrentDirectory fn j
        | fn' == fp2fn "" = j
        | otherwise =
            case breakOnDir fn' of
            Nothing -> do c <- readroot
                          case geta D fn' c of
                            Nothing -> fail "dir doesn't exist in mWithCurrentDirectory..."
                            Just h -> do (h',x) <- withh h j
                                         writeroot $ seta D fn' h' c
                                         return x
            Just (d,fn'') -> do c <- readroot
                                case geta D d c of
                                  Nothing -> fail "dir doesn't exist..."
                                  Just h -> do (h',x) <- withh h $ mWithCurrentDirectory fn'' j
                                               writeroot $ seta D d h' c
                                               return x
        where fn' = normPath fn
    mSetFileExecutable _ _ = return ()
    mWriteFilePS fn ps = do mexists <- identifyThing fn
                            case mexists of
                              Just (D,_) -> fail "can't write file over directory"
                              _ -> do h <- writeHashFile ps
                                      makeThing fn (F,h)
    mCreateDirectory fn = do h <- writeHashFile B.empty
                             exists <- isJust `fmap` identifyThing fn
                             when exists $ fail "can't mCreateDirectory over an existing object."
                             makeThing fn (D,h)
    mRename o n = do nexists <- isJust `fmap` identifyThing n
                     when nexists $ fail "mRename failed..."
                     mx <- identifyThing o
                     -- for backwards compatibility accept rename of nonexistent files.
                     case mx of Nothing -> return ()
                                Just x -> do rmThing o
                                             makeThing n x
    mRemoveDirectory = rmThing
    mRemoveFile f = do x <- mReadFilePS f
                       when (B.length x /= 0) $
                            fail $ "Cannot remove non-empty file "++fn2fp f
                       rmThing f

identifyThing :: FileName -> HashedIO r p (Maybe (ObjType,String))
identifyThing fn | fn' == fp2fn "" = do h <- gets rootHash
                                        return $ Just (D, h)
                 | otherwise = case breakOnDir fn' of
                               Nothing -> getany fn' `fmap` readroot
                               Just (d,fn'') -> do c <- readroot
                                                   case geta D d c of
                                                     Nothing -> return Nothing
                                                     Just h -> inh h $ identifyThing fn''
        where fn' = normPath fn

makeThing :: FileName -> (ObjType,String) -> HashedIO RW p ()
makeThing fn (o,h) = mWithCurrentDirectory (superName $ normPath fn) $
                     seta o (ownName $ normPath fn) h `fmap` readroot >>= writeroot

rmThing :: FileName -> HashedIO RW p ()
rmThing fn = mWithCurrentDirectory (superName $ normPath fn) $
             do c <- readroot
                let c' = filter (\(_,x,_)->x/= ownName (normPath fn)) c
                if length c' == length c - 1
                  then writeroot c'
                  else fail "obj doesn't exist in rmThing"

readhash :: String -> HashedIO r p B.ByteString
readhash h = do c <- gets cache
                z <- lift $ unsafeInterleaveIO $ readHashFile c HashedPristineDir h
                let (_,out) = z
                return out

withh :: String -> HashedIO RW p a -> HashedIO RW p (String,a)
withh h j = do hd <- get
               put $ hd { rootHash = h }
               x <- j
               h' <- gets rootHash
               put hd
               return (h',x)

inh :: String -> HashedIO r p a -> HashedIO r p a
inh h j = do hd <- get
             put $ hd { rootHash = h }
             x <- j
             put hd
             return x

readroot :: HashedIO r p [(ObjType, FileName, String)]
readroot = do haveitalready <- peekroot
              cc <- gets rootHash >>= readdir
              when (not haveitalready) $ speculate cc
              return cc
    where speculate :: [(a,b,String)] -> HashedIO r q ()
          speculate c = do cac <- gets cache
                           mapM_ (\(_,_,z) -> lift $ speculateFileUsingCache cac HashedPristineDir z) c
          peekroot :: HashedIO r p Bool
          peekroot = do HashDir _ c _ h <- get
                        lift $ peekInCache c HashedPristineDir h

writeroot :: [(ObjType, FileName, String)] -> HashedIO r p ()
writeroot c = do h <- writedir c
                 modify $ \hd -> hd { rootHash = h }

data ObjType = F | D deriving Eq

-- | @geta objtype name stuff@ tries to get an object of type @objtype@ named @name@
-- in @stuff@.
geta :: ObjType -> FileName -> [(ObjType, FileName, String)] -> Maybe String
geta o f c = do (o',h) <- getany f c
                if o == o' then Just h else Nothing

getany :: FileName -> [(ObjType, FileName, String)] -> Maybe (ObjType,String)
getany _ [] = Nothing
getany f ((o,f',h):_) | f == f' = Just (o,h)
getany f (_:r) = getany f r

seta :: ObjType -> FileName -> String -> [(ObjType, FileName, String)] -> [(ObjType, FileName, String)]
seta o f h [] = [(o,f,h)]
seta o f h ((_,f',_):r) | f == f' = (o,f,h):r
seta o f h (x:xs) = x : seta o f h xs

readdir :: String -> HashedIO r p [(ObjType, FileName, String)]
readdir hash = (parsed . linesPS) `fmap` readhash hash
    where parsed (t:n:h:rest) | t == dir = (D, niceps2fn n, BC.unpack h) : parsed rest
                              | t == file = (F, niceps2fn n, BC.unpack h) : parsed rest
          parsed _ = []
dir :: B.ByteString
dir = BC.pack "directory:"
file :: B.ByteString
file = BC.pack "file:"


writedir :: [(ObjType, FileName, String)] -> HashedIO r p String
writedir c = writeHashFile cps
    where cps = unlinesPS $ concatMap (\ (o,d,h) -> [showO o,fn2niceps d,BC.pack h]) c++[B.empty]
          showO D = dir
          showO F = file

writeHashFile :: B.ByteString -> HashedIO r p String
writeHashFile ps = do c <- gets cache
                      compr <- gets compress
                      lift $ writeFileUsingCache c compr HashedPristineDir ps

copyHashed :: String -> Cache -> Compression -> String -> IO ()
-- Warning:  A do-notation statement discarded a result of type ((), HashDir RO ghc-prim
copyHashed k c compr z = do _ <- runStateT cph $ HashDir { permissions = RO, cache = c,
                                                      compress = compr, rootHash = z }
                            return ()
    where cph = do cc <- readroot
                   lift $ tediousSize k (length cc)
                   mapM_ cp cc
          cp (F,n,h) = do ps <- readhash h
                          lift $ finishedOneIO k (fn2fp n)
                          lift $ writeAtomicFilePS (fn2fp n) ps
          cp (D,n,h) = do lift $ createDirectoryIfMissing False (fn2fp n)
                          lift $ finishedOneIO k (fn2fp n)
                          lift $ withCurrentDirectory (fn2fp n) $ copyHashed k c compr h

copyPartialsHashed :: FilePathLike fp =>
                      Cache -> Compression -> String -> [fp] -> IO ()
copyPartialsHashed c compr root = mapM_ (copyPartialHashed c compr root)

copyPartialHashed :: FilePathLike fp => Cache -> Compression -> String -> fp -> IO ()
copyPartialHashed c compr root ff =
    do createDirectoryIfMissing True (basename $ toFilePath ff)
-- Warning:  A do-notation statement discarded a result of type ((), HashDir RO ghc-prim
       _ <- runStateT (cp $ fp2fn $ toFilePath ff) $
                 HashDir { permissions = RO, cache = c,
                           compress=compr, rootHash = root }
       return ()
 where basename = reverse . dropWhile ('/' /=) . dropWhile ('/' ==) . reverse
       cp f = do mt <- identifyThing f
                 case mt of
                   Just (D,h) -> do lift $ createDirectoryIfMissing True (fn2fp f)
                                    lift $ withCurrentDirectory (fn2fp f) $ copyHashed "" c compr h
                   Just (F,h) -> do ps <- readhash h
                                    lift $ writeAtomicFilePS (fn2fp f) ps
                   Nothing -> return ()

cleanHashdir :: Cache -> HashedDir -> [String] -> IO ()
cleanHashdir c dir_ hashroots =
   do -- we'll remove obsolete bits of "dir"
      debugMessage $ "Cleaning out " ++ (hashedDir dir_) ++ "..."
      let hashdir = darcsdir ++ "/" ++ (hashedDir dir_) ++ "/"
          listone h = do let size = decodeDarcsSize $ BC.pack h
                             hash = decodeDarcsHash $ BC.pack h
                         x <- readDarcsHashedDir hashdir (size, hash)
                         let subs = [ fst $ darcsLocation "" (s, h') | (TreeType, _, s, h') <- x ]
                             hashes = h : [ fst $ darcsLocation "" (s, h') | (_, _, s, h') <- x ]
                         (hashes++) . concat <$> mapM listone subs
      hs <- set . concat <$> mapM listone hashroots
      fs <- set . filter okayHash <$> getDirectoryContents hashdir
      mapM_ (removeFileMayNotExist . (hashdir++)) (unset $ fs `Set.difference` hs)
      -- and also clean out any global caches.
      debugMessage "Cleaning out any global caches..."
      cleanCachesWithHint c dir_ (unset $ fs `Set.difference` hs)
   where set = Set.fromList . map BC.pack
         unset = map BC.unpack . Set.toList
