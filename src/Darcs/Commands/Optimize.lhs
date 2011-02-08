%  Copyright (C) 2003-2005 David Roundy
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; see the file COPYING.  If not, write to
%  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%  Boston, MA 02110-1301, USA.

\darcsCommand{optimize}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Optimize ( optimize ) where
import Control.Applicative ( (<$>) )
import Control.Exception ( finally )
import Control.Monad ( when, unless )
import Data.Maybe ( isJust )
import Data.List ( sort )
import System.Directory ( getDirectoryContents, doesDirectoryExist,
                          doesFileExist, renameFile, getModificationTime )
import System.IO.Unsafe ( unsafeInterleaveIO )
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL

import Storage.Hashed.Darcs( decodeDarcsSize )

import Darcs.Patch.PatchInfoAnd ( info, extractHash )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag( UpgradeFormat, UseHashedInventory,
                                    Compress, UnCompress,
                                    NoCompress, Reorder,
                                    Relink, OptimizePristine,
                                    OptimizeHTTP ),
                        reorderPatches,
                        uncompressNocompress,
                        relink, sibling,
                        flagsToSiblings,
                        upgradeFormat,
                        workingRepoDir, umaskOption, optimizePristine,
                        optimizeHTTP
                      )
import Darcs.Repository.Prefs ( getPreflist )
import Darcs.Repository ( Repository,
                          withRepoLock, RepoJob(..), withGutsOf,
                          readRepo, optimizeInventory,
                          tentativelyReplacePatches, cleanRepository,
                          amInRepository, finalizeRepositoryChanges, replacePristine )
import Darcs.Witnesses.Ordered ( (+<+), reverseRL, mapRL, (:>)(..)
                               , mapFL, bunchFL, lengthRL )
import Darcs.Patch.Info ( isTag )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( PatchSet(..), newset2RL, newset2FL, progressPatchSet )
import ByteStringUtils ( gzReadFilePS )
import Darcs.Patch.Depends ( splitOnTag )
import Darcs.Lock ( maybeRelink, gzWriteAtomicFilePS, writeAtomicFilePS )
import Darcs.RepoPath ( toFilePath )
import Darcs.Utils ( withCurrentDirectory )
import Progress ( debugMessage )
import Darcs.Global ( darcsdir )

-- imports for optimize --upgrade; to be tidied
import System.Directory ( createDirectoryIfMissing, removeFile )
import System.FilePath.Posix ( takeExtension, (</>), (<.>), takeFileName )

import Progress ( beginTedious, endTedious, tediousSize )
import Darcs.Flags ( compression )
import Darcs.Lock ( rmRecursive )
import Darcs.ProgressPatches ( progressFL )
import Darcs.Repository.Cache ( hashedDir, HashedDir(HashedPristineDir) )
import Darcs.Repository.Format ( identifyRepoFormat,
                                 createRepoFormat, writeRepoFormat, formatHas,
                                 RepoProperty ( HashedInventory ) )
import qualified Darcs.Repository.HashedRepo as HashedRepo
import Darcs.Repository.Prefs ( getCaches )
import Darcs.Repository.Repair ( replayRepository, RepositoryConsistency(..) )
import Darcs.Repository.State ( readRecorded )
import Darcs.Utils ( catchall )

import Storage.Hashed.Tree( TreeItem(..), list, expand, emptyTree )
import Storage.Hashed.AnchoredPath( anchorPath )
import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.Darcs( writeDarcsHashed )

import Codec.Archive.Tar ( write )
import Codec.Archive.Tar.Entry ( fileEntry, toTarPath )
import Codec.Compression.GZip ( compress )

#include "gadts.h"

optimizeDescription :: String
optimizeDescription = "Optimize the repository."

optimizeHelp :: String
optimizeHelp =
 "The `darcs optimize' command modifies the current repository in an\n" ++
 "attempt to reduce its resource requirements.  By default a single\n" ++
 "fast, safe optimization is performed; additional optimization\n" ++
 "techniques can be enabled by passing options to `darcs optimize'.\n" ++
 "\n" ++ optimizeHelpInventory ++
 -- "\n" ++ optimize_help_reorder ++
 "\n" ++ optimizeHelpRelink ++
 -- uncompression is least useful, so it is last.
 "\n" ++ optimizeHelpCompression ++
 "\n" ++
 "There is one more optimization which CAN NOT be performed by this\n" ++
 "command.  Every time your record a patch, a new inventory file is\n" ++
 "written to _darcs/inventories/, and old inventories are never reaped.\n" ++
 "\n" ++
 "If _darcs/inventories/ is consuming a relatively large amount of\n" ++
 "space, you can safely reclaim it by using `darcs get' to make a\n" ++
 "complete copy of the repo.  When doing so, don't forget to copy over\n" ++
 "any unsaved changes you have made to the working tree or to\n" ++
 "unversioned files in _darcs/prefs/ (such as _darcs/prefs/author).\n"

optimize :: DarcsCommand
optimize = DarcsCommand {commandProgramName = "darcs",
                         commandName = "optimize",
                         commandHelp = optimizeHelp,
                         commandDescription = optimizeDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandCommand = optimizeCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [uncompressNocompress, umaskOption],
                         commandBasicOptions = [workingRepoDir,
                                                 reorderPatches,
                                                 sibling, relink,
                                                  upgradeFormat,
                                                 optimizePristine,
                                                 optimizeHTTP]}

optimizeCmd :: [DarcsFlag] -> [String] -> IO ()
optimizeCmd origopts _ = do
    when (UpgradeFormat `elem` origopts) optimizeUpgradeFormat
    withRepoLock opts $ RepoJob $ \repository -> do
    when (OptimizeHTTP `elem` origopts) $ doOptimizeHTTP repository
    if (OptimizePristine `elem` opts)
       then doOptimizePristine repository
       else do cleanRepository repository
               doReorder opts repository
               doOptimizeInventory repository
               when (Compress `elem` opts || UnCompress `elem` opts) $
                    optimizeCompression opts
               when (Relink `elem` opts) $
                    doRelink opts
    putStrLn "Done optimizing!"
  where opts = if UnCompress `elem` origopts then NoCompress:origopts else origopts

optimizeHelpInventory :: String
optimizeHelpInventory =
 "The default optimization moves recent patches (those not included in\n" ++
 "the latest tag) to the `front', reducing the amount that a typical\n" ++
 "remote command needs to download.  It should also reduce the CPU time\n" ++
 "needed for some operations.\n"

doOptimizeInventory :: RepoPatch p => Repository p C(r u t) -> IO ()
doOptimizeInventory repository = do
    debugMessage "Writing out a nice copy of the inventory."
    optimizeInventory repository
    debugMessage "Done writing out a nice copy of the inventory."

optimizeHelpCompression :: String
optimizeHelpCompression =
 "By default patches are compressed with zlib (RFC 1951) to reduce\n" ++
 "storage (and download) size.  In exceptional circumstances, it may be\n" ++
 "preferable to avoid compression.  In this case the `--dont-compress'\n" ++
 "option can be used (e.g. with `darcs record') to avoid compression.\n" ++
 "\n" ++
 "The `darcs optimize --uncompress' and `darcs optimize --compress'\n" ++
 "commands can be used to ensure existing patches in the current\n" ++
 "repository are respectively uncompressed or compressed.  Note that\n" ++
 "repositories in the legacy `old-fashioned-inventory' format have a .gz\n" ++
 "extension on patch files even when uncompressed.\n"

optimizeCompression :: [DarcsFlag] -> IO ()
optimizeCompression opts = do
    putStrLn "Optimizing (un)compression of patches..."
    do_compress (darcsdir++"/patches")
    putStrLn "Optimizing (un)compression of inventories..."
    do_compress (darcsdir++"/inventories")
    where do_compress f =
              do isd <- doesDirectoryExist f
                 if isd then withCurrentDirectory f $
                             do fs <- filter notdot `fmap` getDirectoryContents "."
                                mapM_ do_compress fs
                        else if Compress `elem` opts
                             then gzReadFilePS f >>= gzWriteAtomicFilePS f
                             else gzReadFilePS f >>= writeAtomicFilePS f
          notdot ('.':_) = False
          notdot _ = True

optimizeHelpRelink :: String
optimizeHelpRelink =
 "The `darcs optimize --relink' command hard-links patches that the\n" ++
 "current repository has in common with its peers.  Peers are those\n" ++
 "repositories listed in _darcs/prefs/sources, or defined with the\n" ++
 "`--sibling' option (which can be used multiple times).\n" ++
 "\n" ++
 "Darcs uses hard-links automatically, so this command is rarely needed.\n" ++
 "It is most useful if you used `cp -r' instead of `darcs get' to copy a\n" ++
 "repository, or if you pulled the same patch from a remote repository\n" ++
 "into multiple local repositories.\n"

doOptimizePristine :: RepoPatch p => Repository p C(r u t) -> IO ()
doOptimizePristine repo = do
  hashed <- doesFileExist $ darcsdir </> "hashed_inventory"
  when hashed $ do
    inv <- BS.readFile (darcsdir </> "hashed_inventory")
    let linesInv = BS.split '\n' inv
    case linesInv of
      [] -> return ()
      (pris_line:_) ->
          let size = decodeDarcsSize $ BS.drop 9 pris_line
           in when (isJust size) $ do putStrLn "Optimizing hashed pristine..."
                                      readRecorded repo >>= replacePristine repo
                                      cleanRepository repo

doRelink :: [DarcsFlag] -> IO ()
doRelink opts =
    do some_siblings <- return (flagsToSiblings opts)
       defrepolist <- getPreflist "defaultrepo"
       siblings <- return (map toFilePath some_siblings ++ defrepolist)
       if null siblings
          then putStrLn "No siblings -- no relinking done."
          else do debugMessage "Relinking patches..."
                  patch_tree <- expand =<< readPlainTree "_darcs/patches"
                  let patches = [ anchorPath "" p | (p, File _) <- list patch_tree ]
                  maybeRelinkFiles siblings patches "_darcs/patches"
                  debugMessage "Done relinking."

maybeRelinkFiles :: [String] -> [String] -> String -> IO ()
maybeRelinkFiles src dst dir =
    mapM_ (maybeRelinkFile src) (map ((dir ++ "/") ++) dst)

maybeRelinkFile :: [String] -> String -> IO ()
maybeRelinkFile [] _ = return ()
maybeRelinkFile (h:t) f =
    do done <- maybeRelink (h ++ "/" ++ f) f
       unless done $
           maybeRelinkFile t f
       return ()


\end{code}
\begin{options}
--reorder-patches
\end{options}

The \verb|--reorder-patches| option causes Darcs to create an optimal
ordering of its internal patch inventory. This may help to produce shorter
`context' lists when sending patches, and may improve performance for some
other operations as well.  You should not run \verb!--reorder-patches! on a
repository from which someone may be simultaneously pulling or getting, as
this could lead to repository corruption.
\begin{code}

-- FIXME: someone needs to grovel through the source and determine
-- just how optimizeInventory differs from doReorder.  The following
-- is purely speculation. --twb, 2009-04
-- optimize_help_reorder :: String
-- optimize_help_reorder =
--  "The `darcs optimize --reorder' command is a more comprehensive version\n" ++
--  "of the default optimization.  It reorders patches with respect to ALL\n" ++
--  "tags, rather than just the latest tag.\n"

doReorder :: RepoPatch p => [DarcsFlag] -> Repository p C(r u r) -> IO ()
doReorder opts _ | not (Reorder `elem` opts) = return ()
doReorder opts repository = do
    debugMessage "Reordering the inventory."
    PatchSet ps _ <- chooseOrder `fmap` readRepo repository
-- Warning:  A do-notation statement discarded a result of type Repository p r u r.
    withGutsOf repository $ do _ <- tentativelyReplacePatches repository (compression opts) $ reverseRL ps
                               finalizeRepositoryChanges repository
    debugMessage "Done reordering the inventory."

chooseOrder :: forall p C(s x) . RepoPatch p => PatchSet p C(s x) -> PatchSet p C(s x)
chooseOrder ps = case filter isTag $ mapRL info $ newset2RL ps of
                  [] -> ps
                  (lt:_) -> case splitOnTag lt ps of
                            PatchSet xs ts :> r -> PatchSet (r+<+xs) ts
\end{code}

The \verb|--upgrade| option for \verb!darcs optimize! performs an inplace
upgrade of your repository to the latest \emph{compatible} format.  Right now
means that darcs 1 old-fashioned repositories will be upgraded to darcs-1
hashed repositories (and notably, not to darcs 2 repositories as that would not
be compatible; see \verb!darcs convert!).

\begin{code}
optimizeUpgradeFormat :: IO ()
optimizeUpgradeFormat = do
  debugMessage $ "Upgrading to hashed..."
  rf <- either fail return =<< identifyRepoFormat "."
  debugMessage $ "Found our format"
  if formatHas HashedInventory rf
     then putStrLn "No action taken because this repository already is hashed."
     else do putStrLn "Checking repository in case of corruption..."
             withRepoLock [] $ RepoJob $ \repository -> do
             state <- replayRepository repository [] return
             case state of
               RepositoryConsistent -> do
                 putStrLn "The repository is consistent."
                 actuallyUpgradeFormat repository
               _repoIsBroken ->
                 putStrLn "Corruption detected! Please run darcs repair first."

actuallyUpgradeFormat :: RepoPatch p => Repository p C(r u t) -> IO ()
actuallyUpgradeFormat repository = do
  -- convert patches/inventory
  patches <- readRepo repository
  let k = "Hashing patch"
  beginTedious k
  tediousSize k (lengthRL $ newset2RL patches)
  let patches' = progressPatchSet k patches
  cache <- getCaches [] "."
  let compr = compression [] -- default compression
  HashedRepo.writeTentativeInventory cache compr patches'
  endTedious k
  -- convert pristine by applying patches
  -- the faster alternative would be to copy pristine, but the apply method is more reliable
  let patchesToApply = progressFL "Applying patch" $ newset2FL $ patches'
  createDirectoryIfMissing False $ darcsdir </> hashedDir HashedPristineDir
-- Warning:  A do-notation statement discarded a result of type Storage.Hashed.Hash.Hash.
  _ <- writeDarcsHashed emptyTree "_darcs/pristine.hashed"
  sequence_ $ mapFL HashedRepo.applyToTentativePristine $ bunchFL 100 patchesToApply
  -- now make it official
  HashedRepo.finalizeTentativeChanges repository compr
  writeRepoFormat (createRepoFormat [UseHashedInventory]) (darcsdir </> "format")
  -- clean out old-fashioned junk
  debugMessage "Cleaning out old-fashioned repository files..."
  removeFile   $ darcsdir </> "inventory"
  removeFile   $ darcsdir </> "tentative_inventory"
  rmRecursive (darcsdir </> "pristine") `catchall` rmRecursive (darcsdir </> "current")
  rmGzsIn (darcsdir </> "patches")
  rmGzsIn (darcsdir </> "inventories")
  let checkpointDir = darcsdir </> "checkpoints"
  hasCheckPoints <- doesDirectoryExist checkpointDir
  when hasCheckPoints $ rmRecursive checkpointDir
  putStrLn "Done upgrading!"
 where
  rmGzsIn dir =
    withCurrentDirectory dir $ do
      gzs <- filter ((== ".gz") . takeExtension) `fmap` getDirectoryContents "."
      mapM_ removeFile gzs

doOptimizeHTTP :: RepoPatch p => Repository p C(r u t) -> IO ()
doOptimizeHTTP repo = flip finally (mapM_ (removeFileIfExists)
  [ darcsdir </> "meta-filelist-inventories"
  , darcsdir </> "meta-filelist-pristine"
  , basicTar <.> "part"
  , patchesTar <.> "part"
  ]) $ do
  rf <- either fail return =<< identifyRepoFormat "."
  unless (formatHas HashedInventory rf) . fail $
    "Unsupported repository format:\n" ++
    "  only hashed repositories can be optimized for HTTP"
  createDirectoryIfMissing False packsDir
  -- pack patchesTar
  ps <- mapRL hashedPatchFileName . newset2RL <$> readRepo repo
  is <- map ((darcsdir </> "inventories") </>) <$> HashedRepo.listInventories
  writeFile (darcsdir </> "meta-filelist-inventories") . unlines $
    map takeFileName is
  BL.writeFile (patchesTar <.> "part") . compress . write =<<
    mapM fileEntry' ((darcsdir </> "meta-filelist-inventories") : ps ++
    reverse is)
  renameFile (patchesTar <.> "part") patchesTar
  -- pack basicTar
  pr <- sortByMTime =<< dirContents "pristine.hashed"
  writeFile (darcsdir </> "meta-filelist-pristine") . unlines $
    map takeFileName pr
  BL.writeFile (basicTar <.> "part") . compress . write =<< mapM fileEntry' (
    [ darcsdir </> "meta-filelist-pristine"
    , darcsdir </> "hashed_inventory"
    ] ++ reverse pr)
  renameFile (basicTar <.> "part") basicTar
 where
  packsDir = darcsdir </> "packs"
  basicTar = packsDir </> "basic.tar.gz"
  patchesTar = packsDir </> "patches.tar.gz"
  fileEntry' x = unsafeInterleaveIO $ do
    content <- BL.fromChunks . return <$> gzReadFilePS x
    tp <- either fail return $ toTarPath False x
    return $ fileEntry tp content
  dirContents d = dirContents' d $ const True
  dirContents' d f = map ((darcsdir </> d) </>) . filter (\x ->
    head x /= '.' && f x) <$> getDirectoryContents (darcsdir </> d)
  hashedPatchFileName x = case extractHash x of
    Left _ -> fail "unexpected unhashed patch"
    Right h -> darcsdir </> "patches" </> h
  sortByMTime xs = map snd . sort <$> mapM (\x -> (\t -> (t, x)) <$>
    getModificationTime x) xs
  removeFileIfExists x = do
    ex <- doesFileExist x
    when ex $ removeFile x
\end{code}
