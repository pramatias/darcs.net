{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Repository.Cache (
                   cacheHash, okayHash, takeHash,
                   Cache(..), CacheType(..), CacheLoc(..), WritableOrNot(..),
                   HashedDir(..), hashedDir,
                   unionCaches, unionRemoteCaches, cleanCaches, cleanCachesWithHint,
                   fetchFileUsingCache, speculateFileUsingCache, speculateFilesUsingCache,
                   writeFileUsingCache,
                   peekInCache,
                   repo2cache,
                   writable, isthisrepo, hashedFilePath, allHashedDirs, compareByLocality,
                   reportBadSources
                 ) where

import Control.Monad ( liftM, when, guard, unless, filterM, forM_ )
import Data.List ( nub, intercalate )
import Data.Maybe ( catMaybes, listToMaybe, isJust )
import System.Directory ( removeFile, doesFileExist, doesDirectoryExist,
                          getDirectoryContents, getPermissions )
import qualified System.Directory as SD ( writable )
import System.Posix.Files ( linkCount, getSymbolicLinkStatus )
import System.IO ( hPutStrLn, stderr )

import Crypt.SHA256 ( sha256sum )

import ByteStringUtils ( gzWriteFilePS, linesPS )
import qualified Data.ByteString as B (length, drop, ByteString )
import qualified Data.ByteString.Char8 as BC (unpack)

import SHA1 ( sha1PS )
import System.Posix.Files ( createLink )
import System.FilePath.Posix ( (</>) )
import System.Directory ( createDirectoryIfMissing )

import Darcs.External ( gzFetchFilePS, fetchFilePS, speculateFileOrUrl, copyFileOrUrl,
                        Cachable( Cachable ) )
import Darcs.Flags ( Compression( .. ), RemoteDarcs(..) )
import Darcs.Global ( darcsdir, addBadSource, isBadSource,
                      addReachableSource, isReachableSource,
                      getBadSourcesList )
import Darcs.Lock ( writeAtomicFilePS, gzWriteAtomicFilePS, withTemp )
import Progress ( progressList, debugMessage, debugFail )
import English ( englishNum, Noun(..), Pronoun(..) )
import Darcs.URL ( isFile, isHttpUrl, isSshUrl )
import Darcs.Utils ( withCurrentDirectory, catchall )
import Darcs.SignalHandler ( catchNonSignal )
import qualified HTTP ( ConnectionError(..) )

data HashedDir = HashedPristineDir | HashedPatchesDir | HashedInventoriesDir
hashedDir :: HashedDir -> String
hashedDir HashedPristineDir = "pristine.hashed"
hashedDir HashedPatchesDir = "patches"
hashedDir HashedInventoriesDir = "inventories"

allHashedDirs :: [HashedDir]
allHashedDirs = [HashedPristineDir, HashedPatchesDir, HashedInventoriesDir]

data WritableOrNot = Writable | NotWritable deriving ( Show )
data CacheType = Repo | Directory deriving ( Eq, Show )
data CacheLoc = Cache { cacheType:: !CacheType, cacheWritable:: !WritableOrNot, cacheSource:: !String }
newtype Cache = Ca [CacheLoc] -- abstract type for hiding cache

instance Eq CacheLoc where
    (Cache Repo _ a) == (Cache Repo _ b) = a == b
    (Cache Directory _ a) == (Cache Directory _ b) = a == b
    _ == _ = False
instance Show CacheLoc where
    show (Cache Repo Writable a) = "thisrepo:" ++ a
    show (Cache Repo NotWritable a) = "repo:" ++ a
    show (Cache Directory Writable a) = "cache:" ++ a
    show (Cache Directory NotWritable a) = "readonly:" ++ a
instance Show Cache where
    show (Ca cs) = unlines $ map show cs

unionCaches :: Cache -> Cache -> Cache
unionCaches (Ca a) (Ca b) = Ca (nub (a++b))

-- | unionRemoteCaches merges caches. It tries to do better than just blindly
--   copying remote cache entries:
--
--   * If remote repository is accessed through network, do not copy any cache
--     entries from it. Taking local entries does not make sense and using
--     network entries can lead to darcs hang when it tries to get to
--     unaccessible host.
--
--   * If remote repositoty is local, copy all network cache entries. For local
--     cache entries if the cache directory exists and is writable it is added
--     as writable cache, if it exists but is not writable it is added as
--     read-only cache.
--
--   This approach should save us from bogus cache entries. One case it does not
--   work very well is when you fetch from partial repository over network.
--   Hopefully this is not a common case.
unionRemoteCaches :: Cache -> Cache -> String -> IO (Cache)
unionRemoteCaches local (Ca remote) repourl
    | isFile repourl =  do f <- filtered
                           return $ local `unionCaches` Ca f
    | otherwise = return local
  where filtered = mapM (\x -> fn x `catchall` return Nothing) remote >>=
                   return . catMaybes
        fn :: CacheLoc -> IO (Maybe CacheLoc)
        fn (Cache Repo Writable _) = return Nothing
        fn c@(Cache t _ url)
          | isFile url = do
              ex <- doesDirectoryExist url
              if ex then do p <- getPermissions url
                            return $ Just $
                              if writable c && SD.writable p
                              then c else Cache t NotWritable url
                    else return Nothing
          | otherwise = return $ Just c

-- | Compares two caches, a remote cache is greater than a local one.
-- The order of the comparison is given by: local < http < ssh
compareByLocality :: CacheLoc -> CacheLoc -> Ordering
compareByLocality (Cache _ _  x) (Cache _ _ y)
  | isLocal x &&  isRemote y  = LT
  | isRemote x && isLocal y = GT
  | isHttpUrl x && isSshUrl y = LT
  | isSshUrl x && isHttpUrl y = GT
  | otherwise = EQ
    where
      isRemote r= isHttpUrl r || isSshUrl r
      isLocal = isFile

repo2cache :: String -> Cache
repo2cache r = Ca [Cache Repo NotWritable r]

-- | 'cacheHash' computes the cache hash (i.e. filename) of a packed string.
cacheHash :: B.ByteString -> String
cacheHash ps = case show (B.length ps) of
                 x | l > 10 -> sha256sum ps
                   | otherwise -> take (10-l) (repeat '0') ++ x ++'-':sha256sum ps
                                        where l = length x

okayHash :: String -> Bool
okayHash s = length s == 40 || length s == 64 || length s == 75

takeHash :: B.ByteString -> Maybe (String, B.ByteString)
takeHash ps = do h <- listToMaybe $ linesPS ps
                 let v = BC.unpack h
                 guard $ okayHash v
                 Just (v, B.drop (B.length h) ps)

checkHash :: String -> B.ByteString -> Bool
checkHash h s | length h == 40 = sha1PS s == h
              | length h == 64 = sha256sum s == h
              | length h == 75 = B.length s == read (take 10 h) && sha256sum s == drop 11 h
              | otherwise = False

-- |@fetchFileUsingCache cache dir hash@ receives a list of caches
--  @cache@, the directory for which that file belongs @dir@ and the
--  @hash@ of the file to fetch.  It tries to fetch the file from one
--  of the sources, trying them in order one by one.  If the file
--  cannot be fetched from any of the sources, this operation fails.
fetchFileUsingCache :: Cache -> HashedDir -> String -> IO (String, B.ByteString)
fetchFileUsingCache = fetchFileUsingCachePrivate Anywhere

writable :: CacheLoc -> Bool
writable (Cache _ NotWritable _) = False
writable (Cache _ Writable _) = True

isthisrepo :: CacheLoc -> Bool
isthisrepo (Cache Repo Writable _) = True
isthisrepo _ = False

-- | @hashedFilePath cachelocation subdir hash@ returns the physical filename of
-- hash @hash@ in the @subdir@ section of @cachelocation@.
hashedFilePath :: CacheLoc -> HashedDir -> String -> String
hashedFilePath (Cache Directory _ d) s f = d ++ "/" ++ (hashedDir s) ++ "/" ++ f
hashedFilePath (Cache Repo _ r) s f =
    r ++ "/"++darcsdir++"/" ++ (hashedDir s) ++ "/" ++ f

-- | @peekInCache cache subdir hash@ tells whether @cache@ and
-- contains an object with hash @hash@ in a writable position.
-- Florent: why do we want it to be in a writable position?
peekInCache :: Cache -> HashedDir -> String -> IO Bool
peekInCache (Ca cache) subdir f = cacheHasIt cache `catchall` return False
    where cacheHasIt [] = return False
          cacheHasIt (c:cs) | not $ writable c = cacheHasIt cs
                            | otherwise = do ex <- doesFileExist $ fn c
                                             if ex then return True
                                                   else cacheHasIt cs
          fn c = hashedFilePath c subdir f

-- | @speculateFileUsingCache cache subdirectory name@ takes note that
-- the file @name@ is likely to be useful soon: pipelined downloads
-- will add it to the (low-priority) queue, for the rest it is a noop.
speculateFileUsingCache :: Cache -> HashedDir -> String -> IO ()
speculateFileUsingCache c sd h = do debugMessage $ "Speculating on "++h
                                    copyFileUsingCache OnlySpeculate c sd h

-- | Note that the files are likely to be useful soon: pipelined downloads will
-- add them to the (low-priority) queue, for the rest it is a noop.
speculateFilesUsingCache :: Cache -> HashedDir -> [String] -> IO ()
speculateFilesUsingCache _ _ [] = return ()
speculateFilesUsingCache cache sd hs =
    do --debugMessage $ "Thinking about speculating on "++unwords hs
       hs' <- filterM (fmap not . peekInCache cache sd) hs
       unless (null hs') $ do debugMessage $ "Speculating on "++unwords hs'
                              copyFilesUsingCache OnlySpeculate cache sd hs'

data OrOnlySpeculate = ActuallyCopy | OnlySpeculate deriving ( Eq )

-- | @copyFileUsingCache oos cache subdir f@ copy or hardlinks the
--   file @f@ to one of the caches in @cache@,usually the global cache.
--   If a writable cache doesn't exist the function will fail.
copyFileUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> String -> IO ()
copyFileUsingCache oos (Ca cache) subdir f =
    do debugMessage $ "I'm doing copyFileUsingCache on "++(hashedDir subdir)++"/"++f
       Just stickItHere <- cacheLoc cache
       createDirectoryIfMissing False (reverse $ dropWhile (/='/') $ reverse stickItHere)
       filterBadSources cache >>= (flip sfuc) stickItHere
    `catchall` return ()
    where cacheLoc [] = return Nothing
          cacheLoc (c:cs) | not $ writable c = cacheLoc cs
                          | otherwise =
              do ex <- doesFileExist $ fn c
                 if ex then fail "Bug in darcs: This exception should be caught in speculateFileUsingCache"
                       else do othercache <- cacheLoc cs
                               case othercache of Just x -> return $ Just x
                                                  Nothing -> return $ Just (fn c)
          sfuc [] _ = return ()
          sfuc (c:cs) out
            | not (writable c) =
              if oos == OnlySpeculate
               then speculateFileOrUrl (fn c) out `catchNonSignal` (\e -> checkCacheReachability (show e) c)
               else copyFileOrUrl DefaultRemoteDarcs (fn c) out Cachable `catchNonSignal` (\e -> checkCacheReachability (show e) c)
            | otherwise = sfuc cs out
          fn c = hashedFilePath c subdir f

copyFilesUsingCache :: OrOnlySpeculate -> Cache -> HashedDir -> [String] -> IO ()
copyFilesUsingCache oos cache subdir hs =
    do forM_ hs $ copyFileUsingCache oos cache subdir


data FromWhere = LocalOnly | Anywhere deriving ( Eq )

-- | Checks if a given cache entry is reachable or not.
-- It receives an error caught during execution and the cache entry.
-- If the caches is not reachable it is blacklisted and not longer tried for
-- the rest of the session. If it is reachable it is whitelisted and future errors with such
-- cache get ignore.
-- To determine reachability:
--  * For a local cache, if the given source doesn't exist anymore, it is blacklisted.
--  * For remote sources if the error is timeout, it is blacklisted, if not,
--    it checks if _darcs/hashed_inventory  exist, if it does, the entry is whitelisted, if
--    it doesn't, it is blacklisted.
checkCacheReachability :: String -> CacheLoc -> IO ()
checkCacheReachability e cache
 | isFile source = do
     reachable <- isReachableSource
     unless (reachable source) $ do
       exist <- doesDirectoryExist source
       if exist
        then
         addReachableSource source
        else
         addBadSource source
 | isHttpUrl source = do
     reachable <- isReachableSource
     unless (reachable source) $ do
            let string = case dropWhile (/='(') e of
                          (_:xs) -> fst (break (==')') xs)
                          _      -> e
            let cerror = case reads string ::[(HTTP.ConnectionError,String)] of
                           [(ce,_)] -> Just ce
                           _        -> Nothing
            if isJust cerror
             then addBadSource source
             else checkFileReachability

 | isSshUrl source = do
   reachable <- isReachableSource
   unless (reachable source) checkFileReachability

 | otherwise = fail $ "unknown transport protocol for: " ++ source
 where source = cacheSource cache
       checkFileReachability = do
         reachable <- checkHashedInventoryReachability cache
         if reachable
          then
           addReachableSource source
          else
           addBadSource source

-- | Returns a list of reachables cache entries,
--   taking out the blacklisted entries
filterBadSources :: [CacheLoc] -> IO [CacheLoc]
filterBadSources cache = do
   badSource <- isBadSource
   return $ filter (not . badSource . cacheSource) cache

-- | Checks if the  _darcs/hashed_inventory exist and is reachable
checkHashedInventoryReachability :: CacheLoc -> IO Bool
checkHashedInventoryReachability cache =
     withTemp $ \tempout -> do
       let f = cacheSource cache </> darcsdir </> "hashed_inventory"
       copyFileOrUrl DefaultRemoteDarcs f tempout Cachable
       return True
     `catchNonSignal` (\_ -> return False)

fetchFileUsingCachePrivate :: FromWhere -> Cache -> HashedDir -> String -> IO (String, B.ByteString)
fetchFileUsingCachePrivate fromWhere (Ca cache) subdir f =
    do when (fromWhere == Anywhere) $ copyFileUsingCache ActuallyCopy (Ca cache) subdir f
       filterBadSources cache >>= ffuc
    `catchall` debugFail ("Couldn't fetch `"++f++"'\nin subdir "++ hashedDir subdir ++
                          " from sources:\n\n"++show (Ca cache))
    where ffuc (c:cs)
           | not (writable c) && (Anywhere == fromWhere || isFile (fn c)) =
              do debugMessage $ "In fetchFileUsingCachePrivate I'm going manually"
                 debugMessage $ "    getting "++f
                 debugMessage $ "    from " ++ fn c
                 x <- gzFetchFilePS (fn c) Cachable
                 if not $ checkHash f x
                    then do x' <- fetchFilePS (fn c) Cachable
                            when (not $ checkHash f x') $
                                 do hPutStrLn stderr $ "Hash failure in " ++ fn c
                                    fail $ "Hash failure in " ++ fn c
                            return (fn c, x')
                    else return (fn c, x) -- FIXME: create links in caches
              `catchNonSignal` (\e -> do
                                      checkCacheReachability (show e) c
                                      filterBadSources cs >>= ffuc)


           | writable c =
              do x1 <- gzFetchFilePS (fn c) Cachable
                 x <- if not $ checkHash f x1
                      then do x2 <- fetchFilePS (fn c) Cachable
                              when (not $ checkHash f x2) $
                                 do hPutStrLn stderr $ "Hash failure in " ++ fn c
                                    removeFile $ fn c
                                    fail $ "Hash failure in " ++ fn c
                              return x2
                      else return x1
                 mapM_ (tryLinking (fn c)) cs
                 return (fn c, x)
              `catchNonSignal` (\ e ->
                                 do createCache c subdir `catchall` return () -- don't worry
                                    checkCacheReachability (show e) c
                                    (fname,x) <- filterBadSources cs >>= ffuc
                                    do createLink fname (fn c)
                                       return (fn c, x)
                                     `catchall`
                                      do gzWriteFilePS (fn c) x `catchall` return ()
                                         return (fname,x))
           | otherwise = ffuc cs

          ffuc [] = debugFail $ "No sources from which to fetch file `"++f++"'\n"++ show (Ca cache)

          tryLinking ff c@(Cache Directory Writable d) =
              do createDirectoryIfMissing False (d++"/"++(hashedDir subdir))
                 createLink ff (fn c)
              `catchall` return ()
          tryLinking _ _ = return ()
          fn c = hashedFilePath c subdir f

createCache :: CacheLoc -> HashedDir -> IO ()
createCache (Cache Directory _ d) subdir =
    createDirectoryIfMissing True (d ++ "/" ++ (hashedDir subdir))
createCache _ _ = return ()

-- | @write compression filename content@ writes @content@ to the file @filename@ according
-- to the policy given by @compression@.
write :: Compression -> String -> B.ByteString -> IO ()
write NoCompression = writeAtomicFilePS
write GzipCompression = gzWriteAtomicFilePS

-- | @writeFileUsingCache cache compression subdir contents@ write the string @contents@ to
-- the directory subdir, except if it is already in the cache, in which case it is a noop.
-- Warning (?) this means that in case of a hash collision, writing using writeFileUsingCache is
-- a noop. The returned value is the filename that was given to the string.
writeFileUsingCache :: Cache -> Compression -> HashedDir -> B.ByteString -> IO String
writeFileUsingCache (Ca cache) compr subdir ps =
    (fetchFileUsingCachePrivate LocalOnly (Ca cache) subdir hash >> return hash) `catchall`
    wfuc cache `catchall`
         debugFail ("Couldn't write `"++hash++"'\nin subdir "++(hashedDir subdir)++" to sources:\n\n"++
                    show (Ca cache))
    where hash = cacheHash ps
          wfuc (c:cs) | not $ writable c = wfuc cs
                      | otherwise = do createCache c subdir
                                       write compr (fn c) ps -- FIXME: create links in caches
                                       return hash
          wfuc [] = debugFail $ "No location to write file `" ++ (hashedDir subdir) ++"/"++hash ++ "'"
          fn c = hashedFilePath c subdir hash

cleanCaches :: Cache -> HashedDir -> IO ()
cleanCaches c d = cleanCachesWithHint' c d Nothing

cleanCachesWithHint :: Cache -> HashedDir -> [String] -> IO ()
cleanCachesWithHint c d h = cleanCachesWithHint' c d (Just h)

cleanCachesWithHint' :: Cache -> HashedDir -> Maybe [String] -> IO ()
cleanCachesWithHint' (Ca cs) subdir hint = mapM_ cleanCache cs
    where cleanCache (Cache Directory Writable d) =
             (withCurrentDirectory (d++"/"++(hashedDir subdir)) $
              do fs' <- getDirectoryContents "."
                 let fs = case hint of
                            Just h -> h
                            Nothing -> fs'
                 mapM_ clean $ progressList ("Cleaning cache "++d++"/"++(hashedDir subdir)) $
                       filter okayHash fs) `catchall` return ()
          cleanCache _ = return ()
          clean f = do lc <- linkCount `liftM` getSymbolicLinkStatus f
                       when (lc < 2) $ removeFile f
                    `catchall` return ()

-- | Prints an error message with a list of bad caches.
reportBadSources :: IO ()
reportBadSources = do
  sources <- getBadSourcesList
  let size = length sources
  unless (null sources) $ do
   hPutStrLn stderr $
    concat [ "\nHINT: I could not reach the following ", englishNum size  (Noun "repository") ":"
           , "\n", (intercalate "\n" (map ("        " ++) sources))
           , "\n      If you're not using ", englishNum size It ", you should probably delete"
           , "\n      the corresponding ", englishNum size (Noun "entry") " from _darcs/prefs/sources."
           ]
