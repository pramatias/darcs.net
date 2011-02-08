-- Copyright (C) 2002-2004 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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

{-# LANGUAGE CPP, ScopedTypeVariables #-}

#include "gadts.h"

module Darcs.Repository
    ( Repository, HashedDir(..), Cache(..), CacheLoc(..), WritableOrNot(..)
    , RepoJob(..), maybeIdentifyRepository, identifyRepositoryFor
    , withRepoLock, withRepoReadLock, withRepository, withRepositoryDirectory
    , withGutsOf, makePatchLazy, writePatchSet, findRepository, amInRepository
    , amNotInRepository, replacePristine
    , withRecorded, readRepo, prefsUrl
    , addToPending, tentativelyAddPatch, tentativelyRemovePatches
    , tentativelyAddToPending, tentativelyReplacePatches, readTentativeRepo
    , tentativelyMergePatches, considerMergeToWorking, revertRepositoryChanges
    , finalizeRepositoryChanges, createRepository, copyRepository
    , copyOldrepoPatches, patchSetToRepository, unrevertUrl, applyToWorking
    , patchSetToPatches, createPristineDirectoryTree
    , createPartialsPristineDirectoryTree, optimizeInventory, cleanRepository
    , getMarkedupFile, PatchSet, SealedPatchSet, PatchInfoAnd
    , setScriptsExecutable, setScriptsExecutablePatches
    , checkUnrelatedRepos, testTentative, testRecorded
    , extractOptions, modifyCache, reportBadSources
    -- * Recorded and unrecorded and pending.
    , readRecorded, readUnrecorded, unrecordedChanges, readPending
    , readRecordedAndPending
    -- * Index.
    , readIndex, invalidateIndex
    ) where

import System.Exit ( ExitCode(..), exitWith )
import Data.List ( isPrefixOf )
import Data.Maybe( catMaybes, isJust )

import Darcs.Repository.State( readRecorded, readUnrecorded, readWorking, unrecordedChanges
                             , readPending, readIndex, invalidateIndex
                             , readRecordedAndPending )

import Darcs.Repository.Internal
    (Repository(..), RepoType(..), RepoJob(..),
     maybeIdentifyRepository, identifyRepositoryFor, identifyDarcsRepository,
     IdentifyRepo(..),
     findRepository, amInRepository, amNotInRepository,
     makePatchLazy,
     withRecorded,
     readRepo, readTentativeRepo,
     prefsUrl,
     withRepoLock, withRepoReadLock, withRepository, withRepositoryDirectory, withGutsOf,
     tentativelyAddPatch, tentativelyRemovePatches, tentativelyAddToPending,
     tentativelyReplacePatches,
     revertRepositoryChanges, finalizeRepositoryChanges,
     unrevertUrl,
     applyToWorking, patchSetToPatches,
     createPristineDirectoryTree, createPartialsPristineDirectoryTree,
     optimizeInventory, cleanRepository,
     getMarkedupFile,
     setScriptsExecutable, setScriptsExecutablePatches,
     testTentative, testRecorded,
     makeNewPending, seekRepo
    )
import Darcs.Repository.Merge( tentativelyMergePatches, considerMergeToWorking )
import Darcs.Repository.Cache ( unionRemoteCaches, fetchFileUsingCache,
                                speculateFileUsingCache, HashedDir(..), Cache(..),
                                CacheLoc(..), WritableOrNot(..), hashedDir ,
                                CacheType(Directory), reportBadSources )
import Darcs.Patch.Set ( PatchSet(..), SealedPatchSet, newset2RL, newset2FL, progressPatchSet )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import URL ( maxPipelineLength )

import Control.Exception ( finally )
import Control.Concurrent ( forkIO )
import Control.Concurrent.MVar ( MVar, newMVar, putMVar, takeMVar )
import Control.Monad ( unless, when )
import System.Directory ( createDirectory, renameDirectory,
                          createDirectoryIfMissing, renameFile,
                          doesFileExist, removeFile, getDirectoryContents,
                          getCurrentDirectory, setCurrentDirectory )
import System.IO ( stderr )
import System.IO.Error ( isAlreadyExistsError )
import System.Posix.Files ( createLink )

import qualified Darcs.Repository.DarcsRepo as DarcsRepo
import qualified Darcs.Repository.HashedRepo as HashedRepo

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, extractHash )
import Darcs.Repository.ApplyPatches ( applyPatches )
import Darcs.Repository.HashedRepo ( applyToTentativePristine, pris2inv, revertTentativeChanges,
                                     copySources )
import Darcs.Repository.InternalTypes ( Pristine(..), extractOptions, modifyCache )
import Darcs.Patch ( RepoPatch, PrimOf )
import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.Prim.V1 ( Prim )

import Darcs.Witnesses.Ordered ( FL(..), RL(..), bunchFL, mapFL, mapRL
                               , lengthRL, (+>+), (:\/:)(..) )
import Darcs.Repository.Format ( RepoProperty ( HashedInventory ), RepoFormat,
                                 createRepoFormat, formatHas, writeRepoFormat,
                                 readfromAndWritetoProblem)
import Darcs.Repository.Prefs ( writeDefaultPrefs )
import Darcs.Repository.Pristine ( createPristine, flagsToPristine, createPristineFromWorking )
import Darcs.Patch.Depends ( areUnrelatedRepos, findUncommon )

import Darcs.Utils ( withCurrentDirectory, catchall, promptYorn )
import Darcs.External ( copyFileOrUrl, Cachable(..), fetchFileLazyPS )
import Progress ( debugMessage, tediousSize, beginTedious, endTedious )
import Darcs.ProgressPatches (progressRLShowTags, progressFL)
import Darcs.Lock ( writeBinFile, writeDocBinFile, rmRecursive, withTemp )
import Darcs.Witnesses.Sealed ( Sealed(..), FlippedSeal(..), flipSeal )

import Darcs.Flags ( DarcsFlag( Quiet, Lazy, Complete,
                                AllowUnrelatedRepos, NoUpdateWorking)
                   , compression, UseIndex(..), ScanKnown(..), remoteDarcs
                   , usePacks )
import Darcs.Global ( darcsdir )
import Darcs.URL ( isFile )
import Darcs.SignalHandler ( catchInterrupt )
import Printer ( Doc, text, hPutDocLn )

import Storage.Hashed.Tree( Tree, emptyTree )
import Storage.Hashed.Hash( encodeBase16 )
import Storage.Hashed.Darcs( writeDarcsHashed, darcsAddMissingHashes )
import Storage.Hashed( writePlainTree )
import ByteStringUtils( gzReadFilePS )

import System.FilePath( (</>), takeFileName, splitPath, joinPath
                      , takeDirectory )
import qualified Codec.Archive.Tar as Tar
import Codec.Compression.GZip ( compress, decompress )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

#include "impossible.h"

createRepository :: [DarcsFlag] -> IO ()
createRepository opts = do
  createDirectory darcsdir `catch`
      (\e-> if isAlreadyExistsError e
            then fail "Tree has already been initialized!"
            else fail $ "Error creating directory `"++darcsdir++"'.")
  cwd <- getCurrentDirectory
  x <- seekRepo
  when (isJust x) $ do
      setCurrentDirectory cwd
      putStrLn $ "WARNING: creating a nested repository."
  let rf = createRepoFormat opts
-- Warning:  A do-notation statement discarded a result of type Pristine.
  _ <- createPristine $ flagsToPristine opts rf
  createDirectory $ darcsdir ++ "/patches"
  createDirectory $ darcsdir ++ "/prefs"
  writeDefaultPrefs
  writeRepoFormat rf (darcsdir++"/format")
  if formatHas HashedInventory rf
      then writeBinFile (darcsdir++"/hashed_inventory") ""
      else DarcsRepo.writeInventory "." (PatchSet NilRL NilRL :: PatchSet (Patch Prim) C(Origin Origin)) -- YUCK!

data RepoSort = Hashed | Old

repoSort :: RepoFormat -> RepoSort
repoSort f
  | formatHas HashedInventory f = Hashed
  | otherwise = Old

copyInventory :: forall p C(r u t). RepoPatch p => Repository p C(r u t) -> IO ()
copyInventory fromRepo@(Repo fromDir opts fromFormat (DarcsRepository _ fromCache)) = do
  toRepo@(Repo toDir opts' toFormat (DarcsRepository toPristine toCache)) <-
    identifyDarcsRepository opts "."
  let (_ :: Repository p C(r u t)) = toRepo --The witnesses are wrong, but cannot escape
  case readfromAndWritetoProblem fromFormat toFormat of
    Just e ->  fail $ "Incompatibility with repository " ++ fromDir ++ ":\n" ++ e
    Nothing -> return ()
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo2 :: Repository p C(r u t)
      toRepo2 = Repo toDir opts' toFormat $ DarcsRepository toPristine toCache2
      copyHashedHashed = HashedRepo.copyRepo toRepo2 (remoteDarcs opts) fromDir
      copyAnyToOld r = withCurrentDirectory toDir $ readRepo r >>=
                            DarcsRepo.writeInventoryAndPatches (compression opts)
  case repoSort fromFormat of
    Hashed -> case repoSort toFormat of
      Hashed -> copyHashedHashed
      Old -> copyAnyToOld fromRepo
    Old -> case repoSort toFormat of
      Hashed -> withCurrentDirectory toDir $ do
                HashedRepo.revertTentativeChanges
                patches <- readRepo fromRepo
                let k = "Copying patch"
                beginTedious k
                tediousSize k (lengthRL $ newset2RL patches)
                let patches' = progressPatchSet k patches
                HashedRepo.writeTentativeInventory toCache (compression opts) patches'
                endTedious k
                HashedRepo.finalizeTentativeChanges toRepo $ compression opts
      Old -> copyOldrepoPatches opts fromRepo toDir

copyOldrepoPatches :: RepoPatch p => [DarcsFlag] -> Repository p C(r u t) -> FilePath -> IO ()
copyOldrepoPatches opts (Repo dir _ _ _) out = do
  Sealed patches <- DarcsRepo.readRepo "." :: IO (SealedPatchSet (Patch Prim) C(Origin))
  FlippedSeal scp <- return $ flipSeal $ newset2RL patches
  DarcsRepo.copyPatches (remoteDarcs opts) dir out $ mapRL info $ scp

copyRepository :: forall p C(r u t). RepoPatch p => Repository p C(r u t) -> IO ()
copyRepository fromRepo@(Repo fromDir opts _ _) = do
  debugMessage "Copying prefs"
  copyFileOrUrl (remoteDarcs opts) (fromDir ++ "/" ++ darcsdir ++ "/prefs/prefs")
    (darcsdir ++ "/prefs/prefs") (MaxAge 600) `catchall` return ()
  -- try packs for remote repositories
  if (not . isFile) fromDir && usePacks opts
    then copyPackedRepository fromRepo
    else copyNotPackedRepository fromRepo

putInfo :: [DarcsFlag] -> Doc -> IO ()
putInfo opts = unless (Quiet `elem` opts) . hPutDocLn stderr

copyNotPackedRepository :: forall p C(r u t). RepoPatch p => Repository p C(r u t) -> IO ()
copyNotPackedRepository fromrepository@(Repo _ opts rffrom _) = do
  copyInventory fromrepository
  debugMessage "Grabbing lock in new repository..."
  withRepoLock opts $ RepoJob $ \torepository@(Repo _ _ rfto _) ->
      if formatHas HashedInventory rffrom && formatHas HashedInventory rfto
      then do debugMessage "Writing working directory contents..."
              createPristineDirectoryTree torepository "."
              fetchPatchesIfNecessary opts torepository `catchInterrupt`
                (putInfo opts $ text "Using lazy repository.")
      else if formatHas HashedInventory rfto
           then do local_patches <- readRepo torepository
                   replacePristine torepository emptyTree
                   let patchesToApply = progressFL "Applying patch" $ newset2FL local_patches
                   sequence_ $ mapFL applyToTentativePristine $ bunchFL 100 patchesToApply
                   finalizeRepositoryChanges torepository
                   debugMessage "Writing working directory contents..."
                   createPristineDirectoryTree torepository "."
           else do readRepo torepository >>= (applyPatches . newset2FL)
                   debugMessage "Writing the pristine"
                   pristineFromWorking torepository

copyPackedRepository :: forall p C(r u t). RepoPatch p =>
  Repository p C(r u t) -> IO ()
copyPackedRepository r =
  -- fallback to no-packs get in case of error
  copyPackedRepository2 r `catchall` copyNotPackedRepository r

copyPackedRepository2 :: forall p C(r u t). RepoPatch p =>
  Repository p C(r u t) -> IO ()
copyPackedRepository2 fromRepo@(Repo fromDir opts _ (DarcsRepository _ fromCache)) = do
  b <- fetchFileLazyPS (fromDir ++ "/" ++ darcsdir ++ "/packs/basic.tar.gz") Uncachable
  Repo toDir _ toFormat (DarcsRepository toPristine toCache) <-
    identifyRepositoryFor fromRepo "."
  toCache2 <- unionRemoteCaches toCache fromCache fromDir
  let toRepo :: Repository p C(r u r) -- In empty repo, t(entative) = r(ecorded)
      toRepo = Repo toDir opts toFormat $ DarcsRepository toPristine toCache2
      fromPacksDir = fromDir ++ "/" ++ darcsdir ++ "/packs/"
  createDirectoryIfMissing False $ darcsdir </> "inventories"
  copySources toRepo fromDir
  Repo _ _ _ (DarcsRepository _ toCache3) <-
    identifyRepositoryFor toRepo "."
  -- unpack inventory & pristine cache
  let isLazy = Lazy `elem` opts
  cleanDir "pristine.hashed"
  removeFile $ darcsdir </> "hashed_inventory"
  unpackBasic toCache3 . Tar.read $ decompress b
  createPristineDirectoryTree toRepo "."
  -- pull new patches
  us <- readRepo toRepo
  them <- readRepo fromRepo
  us' :\/: them' <- return $ findUncommon us them
  revertTentativeChanges
  Sealed pw <- tentativelyMergePatches toRepo "get" opts us' them'
  invalidateIndex toRepo
  withGutsOf toRepo $ do
    finalizeRepositoryChanges toRepo
    _ <- applyToWorking toRepo opts pw
    return ()
  -- get old patches
  unless isLazy $ (do
    cleanDir "patches"
    putInfo opts $ text "Copying patches, to get lazy repository hit ctrl-C..."
    unpackPatches toCache3 (mapFL hashedPatchFileName $ newset2FL us) .
      Tar.read . decompress =<< fetchFileLazyPS (fromPacksDir ++
      "patches.tar.gz") Uncachable
    ) `catchInterrupt` (putInfo opts $ text "Using lazy repository.")
 where
  cleanDir d = mapM_ (\x -> removeFile $ darcsdir </> d </> x) .
    filter (\x -> head x /= '.') =<< getDirectoryContents (darcsdir </> d)

withControlMVar :: (MVar () -> IO ()) -> IO ()
withControlMVar f = do
  mv <- newMVar ()
  f mv
  takeMVar mv

forkWithControlMVar :: MVar () -> IO () -> IO ()
forkWithControlMVar mv f = do
  takeMVar mv
  _ <- forkIO $ flip finally (putMVar mv ()) f
  return ()

removeMetaFiles :: IO ()
removeMetaFiles = mapM_ (removeFile . (darcsdir </>)) .
  filter ("meta-" `isPrefixOf`) =<< getDirectoryContents darcsdir

unpackBasic :: Cache -> Tar.Entries -> IO ()
unpackBasic c x = do
  withControlMVar $ \mv -> unpackTar c (basicMetaHandler c mv) x
  removeMetaFiles

unpackPatches :: Cache -> [String] -> Tar.Entries -> IO ()
unpackPatches c ps x = do
  withControlMVar $ \mv -> unpackTar c (patchesMetaHandler c ps mv) x
  removeMetaFiles

unpackTar :: Cache -> IO () -> Tar.Entries -> IO ()
unpackTar  _ _ Tar.Done = return ()
unpackTar  _ _ (Tar.Fail e)= fail e
unpackTar c mh (Tar.Next x xs) = case Tar.entryContent x of
  Tar.NormalFile x' _ -> do
    let p = Tar.entryPath x
    if "meta-" `isPrefixOf` takeFileName p
      then do
        BL.writeFile p x'
        mh
        unpackTar c mh xs
      else do
        ex <- doesFileExist p
        if ex
          then debugMessage $ "Tar thread: STOP " ++ p
          else do
            if p == darcsdir </> "hashed_inventory"
              then writeFile' Nothing p x'
              else writeFile' (cacheDir c) p $ compress x'
            debugMessage $ "Tar thread: GET " ++ p
            unpackTar c mh xs
  _ -> fail "Unexpected non-file tar entry"
 where
  writeFile' Nothing z y = withTemp $ \x' -> do
    BL.writeFile x' y
    renameFile x' z
  writeFile' (Just ca) z y = do
    let x' = joinPath . tail $ splitPath z -- drop darcsdir
    ex <- doesFileExist $ ca </> x'
    if ex
      then createLink' (ca </> x') z
      else withTemp $ \x'' -> do
        BL.writeFile x'' y
        createLink' x'' $ ca </> x'
        renameFile x'' z
  createLink' z y = do
    createDirectoryIfMissing True $ takeDirectory y
    createLink z y `catchall` return ()

basicMetaHandler :: Cache -> MVar () -> IO ()
basicMetaHandler ca mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-pristine"
  when ex . forkWithControlMVar mv $
    fetchFilesUsingCache ca HashedPristineDir . lines =<<
      readFile (darcsdir </> "meta-filelist-pristine")
  return ()

patchesMetaHandler :: Cache -> [String] -> MVar () -> IO ()
patchesMetaHandler ca ps mv = do
  ex <- doesFileExist $ darcsdir </> "meta-filelist-inventories"
  when ex $ do
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPristineDir .
      lines =<< readFile (darcsdir </> "meta-filelist-inventories")
    forkWithControlMVar mv $ fetchFilesUsingCache ca HashedPatchesDir ps
  return ()

cacheDir :: Cache -> Maybe String
cacheDir (Ca cs) = safeHead . catMaybes .flip map cs $ \x -> case x of
  Cache Directory Writable x' -> Just x'
  _ -> Nothing

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

hashedPatchFileName :: PatchInfoAnd p C(a b) -> String
hashedPatchFileName x = case extractHash x of
  Left _ -> fail "unexpected unhashed patch"
  Right h -> h
 
-- | fetchFilesUsingCache is similar to mapM fetchFileUsingCache, exepts
-- it stops execution if file it's going to fetch already exists.
fetchFilesUsingCache :: Cache -> HashedDir -> [FilePath] -> IO ()
fetchFilesUsingCache _ _ [] = return ()
fetchFilesUsingCache c d (f:fs) = do
  ex <- doesFileExist $ darcsdir </> hashedDir d </> f
  if ex
    then debugMessage $ "Cache thread: STOP " ++
      (darcsdir </> hashedDir d </> f)
    else do
      debugMessage $ "Cache thread: GET " ++
        (darcsdir </> hashedDir d </> f)
-- Warning:  A do-notation statement discarded a result of type (String, BS.ByteString).
      _ <- fetchFileUsingCache c d f
      fetchFilesUsingCache c d fs

-- | writePatchSet is like patchSetToRepository, except that it doesn't
-- touch the working directory or pristine cache.
writePatchSet :: RepoPatch p => PatchSet p C(Origin x) -> [DarcsFlag] -> IO (Repository p C(r u t))
writePatchSet patchset opts = do
    maybeRepo <- maybeIdentifyRepository opts "."
    let repo@(Repo _ _ rf2 (DarcsRepository _ c)) =
          case maybeRepo of
            GoodRepository r -> r
            BadRepository e -> bug ("Current directory is a bad repository in writePatchSet: " ++ e)
            NonRepository e -> bug ("Current directory not a repository in writePatchSet: " ++ e)
    debugMessage "Writing inventory"
    if formatHas HashedInventory rf2
       then do HashedRepo.writeTentativeInventory c (compression opts) patchset
               HashedRepo.finalizeTentativeChanges repo (compression opts)
       else DarcsRepo.writeInventoryAndPatches (compression opts) patchset
    return repo

-- | patchSetToRepository takes a patch set, and writes a new repository in the current directory
--   that contains all the patches in the patch set. This function is used when 'darcs get'ing a
--   repository with the --to-match flag and the new repository is not in hashed format.
--   This function does not (yet) work for hashed repositories. If the passed @DarcsFlag@s tell
--   darcs to create a hashed repository, this function will call @error@.
patchSetToRepository :: RepoPatch p => Repository p C(r1 u1 r1) -> PatchSet p C(Origin x)
                     -> [DarcsFlag] -> IO (Repository p C(r u t))
patchSetToRepository (Repo fromrepo _ rf _) patchset opts = do
    when (formatHas HashedInventory rf) $ -- set up sources and all that
       do writeFile "_darcs/tentative_pristine" "" -- this is hokey
          repox <- writePatchSet patchset opts
          HashedRepo.copyRepo repox (remoteDarcs opts) fromrepo
    repo <- writePatchSet patchset opts
    readRepo repo >>= (applyPatches . newset2FL)
    debugMessage "Writing the pristine"
    pristineFromWorking repo
    return repo

checkUnrelatedRepos :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x) -> PatchSet p C(start y)
                    -> IO ()
checkUnrelatedRepos opts _ _ | AllowUnrelatedRepos `elem` opts = return ()
checkUnrelatedRepos _ us them =
    if areUnrelatedRepos us them
    then do yorn <- promptYorn ("Repositories seem to be unrelated. Proceed?")
            when (yorn /= 'y') $ do putStrLn "Cancelled."
                                    exitWith ExitSuccess
    else return ()

-- | Unless a flag has been given in the first argument that tells darcs not to do so (--lazy,
--   or --partial), this function fetches all patches that the given repository has
--   with fetchFileUsingCache. This is used as a helper in copyRepository.
fetchPatchesIfNecessary :: forall p C(r u t). RepoPatch p => [DarcsFlag] -> Repository p C(r u t) -> IO ()
fetchPatchesIfNecessary opts torepository@(Repo _ _ _ (DarcsRepository _ c)) =
    unless (Lazy `elem` opts) $
             do unless (Complete `elem` opts) $
                       putInfo opts $ text "Copying patches, to get lazy repository hit ctrl-C..."
                r <- readRepo torepository
                pipelineLength <- maxPipelineLength
                let patches = newset2RL r
                    ppatches = progressRLShowTags "Copying patches" patches
                    (first, other) = splitAt (pipelineLength - 1) $ tail $ hashes patches
                    speculate | pipelineLength > 1 = [] : first : map (:[]) other
                              | otherwise = []
                mapM_ fetchAndSpeculate $ zip (hashes ppatches) (speculate ++ repeat [])
  where hashes :: FORALL(x y) RL (PatchInfoAnd p) C(x y) -> [String]
        hashes = catMaybes . mapRL ((either (const Nothing) Just) . extractHash)
        fetchAndSpeculate :: (String, [String]) -> IO ()
        fetchAndSpeculate (f, ss) = do
-- Warning:  A do-notation statement discarded a result of type (String, BS.ByteString).
          _ <- fetchFileUsingCache c HashedPatchesDir f
          mapM_ (speculateFileUsingCache c HashedPatchesDir) ss

addToPending :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(u y) -> IO ()
addToPending (Repo _ opts _ _) _ | NoUpdateWorking `elem` opts = return ()
addToPending repo@(Repo{}) p =
    do pend <- unrecordedChanges (UseIndex, ScanKnown) repo Nothing
       invalidateIndex repo
       makeNewPending repo (pend +>+ p)

-- | Replace the existing pristine with a new one (loaded up in a Tree object).
replacePristine :: Repository p C(r u t) -> Tree IO -> IO ()
replacePristine (Repo r _opts _rf (DarcsRepository pris _c)) tree =
    withCurrentDirectory r $ replace pris
    where replace HashedPristine =
              do let t = darcsdir </> "hashed_inventory"
                 i <- gzReadFilePS t
                 tree' <- darcsAddMissingHashes tree
                 root <- writeDarcsHashed tree' $ darcsdir </> "pristine.hashed"
                 writeDocBinFile t $ pris2inv (BS.unpack $ encodeBase16 root) i
          replace (PlainPristine n) =
              do rmRecursive nold `catchall` return ()
                 writePlainTree tree ntmp
                 renameDirectory n nold
                 renameDirectory ntmp n
                 return ()
          replace NoPristine = return ()
          nold = darcsdir </> "pristine-old"
          ntmp = darcsdir </> "pristine-tmp"

pristineFromWorking :: RepoPatch p => Repository p C(r u t) -> IO ()
pristineFromWorking repo@(Repo dir _ rf _)
    | formatHas HashedInventory rf =
        withCurrentDirectory dir $ readWorking >>= replacePristine repo
pristineFromWorking (Repo dir _ _ (DarcsRepository p _)) =
  withCurrentDirectory dir $ createPristineFromWorking p
