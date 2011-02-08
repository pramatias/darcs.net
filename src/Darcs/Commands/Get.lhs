%  Copyright (C) 2002-2005 David Roundy
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

\darcsCommand{get}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Get ( get, clone ) where

import System.Directory ( setCurrentDirectory, doesDirectoryExist, doesFileExist,
                          createDirectory )
import Workaround ( getCurrentDirectory )
import Control.Monad ( when )

import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias, putInfo )
import Darcs.Flags( compression, remoteDarcs )
import Darcs.Arguments ( DarcsFlag( NewRepo, Lazy,
                                    UseFormat2, UseOldFashionedInventory, UseHashedInventory,
                                    SetScriptsExecutable, OnePattern ),
                        getContext, getInventoryChoices,
                        partial, reponame,
                        matchOneContext, setDefault, setScriptsExecutableOption,
                        networkOptions, makeScriptsExecutable, usePacks )
import Darcs.Repository ( Repository, withRepository, RepoJob(..), withRepoLock, identifyRepositoryFor, readRepo,
                          createPristineDirectoryTree,
                          tentativelyRemovePatches, patchSetToRepository,
                          copyRepository, tentativelyAddToPending,
                          finalizeRepositoryChanges, setScriptsExecutable
                        , invalidateIndex )
import Darcs.Repository.Format ( identifyRepoFormat, RepoFormat,
                                 RepoProperty ( Darcs2, HashedInventory ), formatHas )
import Darcs.Repository.DarcsRepo ( writeInventory )
import qualified Darcs.Repository.DarcsRepo as DR ( readRepo )
import Darcs.Repository ( SealedPatchSet, copyOldrepoPatches,
                        createRepository)
import Darcs.Patch.Set ( PatchSet(..),  newset2RL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Repository.ApplyPatches ( applyPatches )
import Darcs.Patch ( RepoPatch, apply, invert, effect )
import Darcs.Patch.V1 ( Patch ) -- needed by some code that should be refactored into Repository
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Witnesses.Ordered ( RL(..), reverseRL, lengthFL, mapFL_FL, (:>)(..) )
import Darcs.External ( copyFileOrUrl, Cachable(..) )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Repository.Prefs ( setDefaultrepo )
import Darcs.Repository.Motd ( showMotd )
import Darcs.Repository.Pristine ( identifyPristine, createPristineFromWorking, )
import Darcs.Commands.Init ( initialize )
import Darcs.Match ( havePatchsetMatch, getOnePatchset )
import Darcs.Utils ( catchall, formatPath, withCurrentDirectory )
import Progress ( debugMessage )
import Printer ( text, errorDoc, ($$) )
import Darcs.Lock ( writeBinFile )
import Darcs.RepoPath ( toFilePath, toPath, ioAbsoluteOrRemote)
import Darcs.Witnesses.Sealed ( Sealed(..) )
import Darcs.Global ( darcsdir )
import English ( englishNum, Noun(..) )
#include "impossible.h"
#include "gadts.h"

getDescription :: String
getDescription = "Create a local copy of a repository."

getHelp :: String
getHelp =
 "Get creates a local copy of a repository.  The optional second\n" ++
 "argument specifies a destination directory for the new copy; if\n" ++
 "omitted, it is inferred from the source location.\n" ++
 "\n" ++
 "By default Darcs will copy every patch from the original repository.\n" ++
 "This means the copy is completely independent of the original; you can\n" ++
 "operate on the new repository even when the original is inaccessible.\n" ++
 "If you expect the original repository to remain accessible, you can\n" ++
 "use --lazy to avoid copying patches until they are needed (`copy on\n" ++
 "demand').  This is particularly useful when copying a remote\n" ++
 "repository with a long history that you don't care about.\n" ++
 "\n" ++
 "The --lazy option isn't as useful for local copies, because Darcs will\n" ++
 "automatically use `hard linking' where possible.  As well as saving\n" ++
 "time and space, you can move or delete the original repository without\n" ++
 "affecting a complete, hard-linked copy.  Hard linking requires that\n" ++
 "the copy be on the same filesystem and the original repository, and\n" ++
 "that the filesystem support hard linking.  This includes NTFS, HFS+\n" ++
 "and all general-purpose Unix filesystems (such as ext3, UFS and ZFS).\n" ++
 "FAT does not support hard links.\n" ++
 "\n" ++
 "Darcs get will not copy unrecorded changes to the source repository's\n" ++
 "working tree.\n" ++
 "\n" ++
 getHelpTag ++
 "\n" ++
 -- The remaining help text covers backwards-compatibility options.
 "A repository created by `darcs get' will be in the best available\n" ++
 "format: it will be able to exchange patches with the source\n" ++
 "repository, but will not be directly readable by Darcs binaries older\n" ++
 "than 2.0.0.  Use the `--old-fashioned-inventory' option if the latter\n" ++
 "is required.\n"

get :: DarcsCommand
get = DarcsCommand {commandProgramName = "darcs",
                    commandName = "get",
                    commandHelp = getHelp,
                    commandDescription = getDescription,
                    commandExtraArgs = -1,
                    commandExtraArgHelp = ["<REPOSITORY>", "[<DIRECTORY>]"],
                    commandCommand = getCmd,
                    commandPrereq = contextExists,
                    commandGetArgPossibilities = return [],
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = networkOptions ++ usePacks :
                                               commandAdvancedOptions initialize,
                    commandBasicOptions = [reponame,
                                            partial,
                                            matchOneContext,
                                            setDefault True,
                                            setScriptsExecutableOption,
                                             getInventoryChoices]}

clone :: DarcsCommand
clone = commandAlias "clone" Nothing get

getCmd :: [DarcsFlag] -> [String] -> IO ()
getCmd opts [inrepodir, outname] = getCmd (NewRepo outname:opts) [inrepodir]
getCmd opts [inrepodir] = do
  debugMessage "Starting work on get..."
  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir
  showMotd opts repodir
  rfsource_or_e <- identifyRepoFormat repodir
  rfsource <- case rfsource_or_e of Left e -> fail e
                                    Right x -> return x
  debugMessage $ "Found the format of "++repodir++"..."
  mysimplename <- makeRepoName opts repodir
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  when (formatHas Darcs2 rfsource && UseOldFashionedInventory `elem` opts) $
    putInfo opts $ text "Warning: 'old-fashioned-inventory' is ignored with a darcs-2 repository\n"
  let opts' = if formatHas Darcs2 rfsource
              then UseFormat2:opts
              else if not (UseOldFashionedInventory `elem` opts)
                   then UseHashedInventory:filter (/= UseFormat2) opts
                   else UseOldFashionedInventory:filter (/= UseFormat2) opts
  createRepository opts'
  debugMessage "Finished initializing new directory."
  setDefaultrepo repodir opts

  rf_or_e <- identifyRepoFormat "."
  rf <- case rf_or_e of Left e -> fail e
                        Right x -> return x
  if formatHas HashedInventory rf -- refactor this into repository
    then writeBinFile (darcsdir++"/hashed_inventory") ""
    else writeInventory "." (PatchSet NilRL NilRL :: PatchSet (Patch Prim) C(Origin Origin))

  if not (null [p | OnePattern p <- opts]) -- --to-match given
     && not (Lazy `elem` opts)
    then withRepository opts $ RepoJob $ \repository -> do
      debugMessage "Using economical get --to-match handling"
      fromrepo <- identifyRepositoryFor  repository repodir
      Sealed patches_to_get <- getOnePatchset fromrepo opts
-- Warning:  A do-notation statement discarded a result of type Repository p ghc-prim
      _ <- patchSetToRepository fromrepo patches_to_get opts
      debugMessage "Finished converting selected patch set to new repository"
    else copyRepoAndGoToChosenVersion opts repodir rfsource rf
getCmd _ _ = fail "You must provide 'get' with either one or two arguments."

-- | called by getCmd
-- assumes that the target repo of the get is the current directory, and that an inventory in the
-- right format has already been created.
copyRepoAndGoToChosenVersion :: [DarcsFlag] -> String -> RepoFormat -> RepoFormat -> IO ()
copyRepoAndGoToChosenVersion opts repodir rfsource rf = do
  copyRepo
  withRepository opts $ RepoJob $ \repository -> goToChosenVersion repository opts
  putInfo opts $ text "Finished getting."
      where copyRepo =
                withRepository opts $ RepoJob $ \repository -> do
                  let hashUs   = formatHas HashedInventory rf
                      hashThem = formatHas HashedInventory rfsource
                  case () of _ | hashUs && hashThem -> do
                                   debugMessage "Identifying and copying repository..."
                                   copyRepoHashed repository
                               | hashUs -> do
                                   putInfo opts $ text "***********************************************************************"
                                               $$ text "  _______   Sorry for the wait! The repository you are fetching is"
                                               $$ text " |       |  using the DEPRECATED 'old-fashioned' format. I'm getting a"
                                               $$ text " | O   O |  hashed copy instead, but this may take a while."
                                               $$ text " |  ___  |"
                                               $$ text " | /   \\ |  We recommend that the maintainer upgrade the remote copy"
                                               $$ text " |_______|  as well because the next major Darcs release (June 2011)"
                                               $$ text "            will not be able to read old-fashioned repositories. The"
                                               $$ text "            hashed format requires a Darcs 2 client, but is otherwise"
                                               $$ text "            compatible with existing old-fashioned branches."
                                               $$ text ""
                                               $$ text "            Upgrading is easy with Darcs 2.4 and higher:"
                                               $$ text "            example.com $ cd path/to/repo"
                                               $$ text "            example.com $ darcs optimize --upgrade"
                                               $$ text "***********************************************************************"
                                   copyRepoHashed repository
                               | hashThem -> do
                                   putInfo opts $ text "Fetching a hashed repository as an old-fashioned one..."
                                   copyRepoHashed repository
                               | otherwise -> copyRepoOldFashioned repository opts repodir
            copyRepoHashed repository =
              do identifyRepositoryFor repository repodir >>= copyRepository
                 when (SetScriptsExecutable `elem` opts) setScriptsExecutable

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName (NewRepo n:_) _ =
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
makeRepoName (_:as) d = makeRepoName as d
makeRepoName [] d =
  case dropWhile (=='.') $ reverse $
       takeWhile (\c -> c /= '/' && c /= ':') $
       dropWhile (=='/') $ reverse d of
  "" -> modifyRepoName "anonymous_repo"
  base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i

getHelpTag :: String
getHelpTag =
 "It is often desirable to make a copy of a repository that excludes\n" ++
 "some patches.  For example, if releases are tagged then `darcs get\n" ++
 "--tag .' would make a copy of the repository as at the latest release.\n" ++
 "\n" ++
 "An untagged repository state can still be identified unambiguously by\n" ++
 "a context file, as generated by `darcs changes --context'.  Given the\n" ++
 "name of such a file, the --context option will create a repository\n" ++
 "that includes only the patches from that context.  When a user reports\n" ++
 "a bug in an unreleased version of your project, the recommended way to\n" ++
 "find out exactly what version they were running is to have them\n" ++
 "include a context file in the bug report.\n" ++
 "\n" ++
 "You can also make a copy of an untagged state using the --to-patch or\n" ++
 "--to-match options, which exclude patches `after' the first matching\n" ++
 "patch.  Because these options treat the set of patches as an ordered\n" ++
 "sequence, you may get different results after reordering with `darcs\n" ++
 "optimize', so tagging is preferred.\n"

contextExists :: [DarcsFlag] -> IO (Either String ())
contextExists opts =
   case getContext opts of
     Nothing -> return $ Right ()
     Just f  -> do exists <- doesFileExist $ toFilePath f
                   if exists
                      then return $ Right ()
                      else return . Left $ "Context file "++toFilePath f++" does not exist"

goToChosenVersion :: RepoPatch p => Repository p C(r u r)
                     -> [DarcsFlag] -> IO ()
goToChosenVersion repository opts =
    when (havePatchsetMatch opts) $ do
       debugMessage "Going to specified version..."
       patches <- readRepo repository
       Sealed context <- getOnePatchset repository opts
       when (snd (countUsThem patches context) > 0) $
            errorDoc $ text "Missing patches from context!" -- FIXME : - (
       _ :> us' <- return $ findCommonWithThem patches context
       let ps = mapFL_FL hopefully us'
       putInfo opts $ text $ "Unapplying " ++ (show $ lengthFL ps) ++ " " ++
                   (englishNum (lengthFL ps) (Noun "patch") "")
       invalidateIndex repository
       withRepoLock opts $ RepoJob $ \_ ->
-- Warning:  A do-notation statement discarded a result of type Repository p r u z.
           do _ <- tentativelyRemovePatches repository (compression opts) us'
              tentativelyAddToPending repository opts $ invert $ effect us'
              finalizeRepositoryChanges repository
              apply (invert $ effect ps) `catch` \e ->
                  fail ("Couldn't undo patch in working dir.\n" ++ show e)
              makeScriptsExecutable opts (invert $ effect ps)

copyRepoOldFashioned :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> String -> IO ()
copyRepoOldFashioned repository opts repodir = do
  myname <- getCurrentDirectory
  fromrepo <- identifyRepositoryFor repository repodir
  patches <- readRepo fromrepo
  debugMessage "Getting the inventory..."
  writeInventory "." patches
  debugMessage "Copying patches..."
  copyOldrepoPatches opts fromrepo "."
  debugMessage "Patches copied"
  Sealed local_patches <- DR.readRepo "." :: IO (SealedPatchSet (Patch Prim) C(Origin))
  debugMessage "Repo read"
  repo_is_local <- doesDirectoryExist repodir
  debugMessage $ "Repo local: " ++ formatPath (show repo_is_local)
  if repo_is_local
     then do
       debugMessage "Copying prefs"
       copyFileOrUrl (remoteDarcs opts)
          (repodir++"/"++darcsdir++"/prefs/prefs") (darcsdir++"/prefs/prefs") (MaxAge 600)
          `catchall` return ()
       debugMessage "Writing working directory"
       createPristineDirectoryTree fromrepo myname
       withCurrentDirectory myname $ do
           -- note: SetScriptsExecutable is normally checked in PatchApply
           -- but darcs get on local repositories does not apply patches
           when (SetScriptsExecutable `elem` opts) setScriptsExecutable
     else do
       setCurrentDirectory myname
       applyPatches $ reverseRL $ newset2RL local_patches
  debugMessage "Writing the pristine"
  pristine <- identifyPristine
  createPristineFromWorking pristine
  setCurrentDirectory myname

\end{code}
