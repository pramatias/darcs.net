%  Copyright (C) 2002-2004 David Roundy
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

\darcsCommand{add}
\begin{code}
module Darcs.Commands.Add ( add, expandDirs ) where

import Data.List ( (\\), nub)
import Data.Maybe( isNothing )
import Control.Monad ( when, unless, liftM )
import Storage.Hashed.Tree( Tree, findTree, expand )
import Storage.Hashed.AnchoredPath( floatPath, anchorPath, parents )

import Darcs.Commands(DarcsCommand(..), putVerbose, putWarning, nodefaults)
import Darcs.Arguments (noskipBoring, allowProblematicFilenames,
                       recursive, workingRepoDir, dryRunNoxml, umaskOption,
                       listFiles, listUnregisteredFiles,
                        DarcsFlag ( Recursive, DryRun, Verbose),
                        fixSubPaths,
                      )
import Darcs.Flags( includeBoring, doAllowCaseOnly, doAllowWindowsReserved,)
import Darcs.Utils ( withCurrentDirectory, nubsort )
import Darcs.Repository.State( readRecordedAndPending )
import Darcs.Repository ( amInRepository, withRepoLock, RepoJob(..), addToPending )
import Darcs.Patch ( Patchy, PrimPatch, applyToTree, addfile, adddir )
import Darcs.Witnesses.Ordered ( FL(..), (+>+), nullFL )
import Darcs.Witnesses.Sealed ( Sealed(..), Gap(..), FreeLeft, unFreeLeft )
import Darcs.Utils ( isFileReallySymlink, doesDirectoryReallyExist
                   , doesFileReallyExist, treeHas, treeHasDir, treeHasAnycase )
import Darcs.RepoPath ( SubPath, toFilePath, simpleSubPath, toPath )
import Darcs.Repository.Prefs ( darcsdirFilter, boringFileFilter )
import Data.Maybe ( maybeToList )
import System.FilePath.Posix ( takeDirectory, (</>) )
import qualified System.FilePath.Windows as WindowsFilePath
import Printer( text )

#include "gadts.h"
#include "impossible.h"

addDescription :: String
addDescription = "Add one or more new files or directories."

addHelp :: String
addHelp =
 "Generally a repository contains both files that should be version\n" ++
 "controlled (such as source code) and files that Darcs should ignore\n" ++
 "(such as executables compiled from the source code).  The `darcs add'\n" ++
 "command is used to tell Darcs which files to version control.\n" ++
 "\n" ++
 "When an existing project is first imported into a Darcs repository, it\n" ++
 "is common to run `darcs add -r *' or `darcs record -l' to add all\n" ++
 "initial source files into darcs.\n"++
 "\n" ++
 "Adding symbolic links (symlinks) is not supported.\n\n"

add :: DarcsCommand
add = DarcsCommand {commandProgramName = "darcs",
                    commandName = "add",
                    commandHelp = addHelp ++ addHelp',
                    commandDescription = addDescription,
                    commandExtraArgs = -1,
                    commandExtraArgHelp = ["<FILE or DIRECTORY> ..."],
                    commandCommand = addCmd,
                    commandPrereq = amInRepository,
                    commandGetArgPossibilities = listUnregisteredFiles,
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = [umaskOption],
                    commandBasicOptions =
                    [noskipBoring, allowProblematicFilenames,
                     recursive "add contents of subdirectories",
                     workingRepoDir, dryRunNoxml]}

addHelp' :: String
addHelp' =
 "Darcs will ignore all files and folders that look `boring'.  The\n" ++
 "--boring option overrides this behaviour.\n" ++
 "\n" ++
 "Darcs will not add file if another file in the same folder has the\n" ++
 "same name, except for case.  The --case-ok option overrides this\n" ++
 "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
 "files a folder to have the same name except for case (for example,\n" ++
 "`ReadMe' and `README').  If --case-ok is used, the repository might be\n" ++
 "unusable on those systems!\n\n"

addCmd :: [DarcsFlag] -> [String] -> IO ()
addCmd opts args
  | null args = putStrLn $ "Nothing specified, nothing added." ++
      "Maybe you wanted to say `darcs add --recursive .'?"
  | otherwise = do
      fs <- fixSubPaths opts args
      case fs of
        [] -> fail "No valid arguments were given"
        _ -> addFiles opts fs

addFiles :: [DarcsFlag] -> [SubPath] -> IO ()
addFiles opts origfiles = withRepoLock opts $ RepoJob $ \repository ->
 do -- TODO do not expand here, and use findM/findIO or such later
    -- (needs adding to hashed-storage first though)
    cur <- expand =<< readRecordedAndPending repository
    let parlist = getParents cur (map toFilePath origfiles)
    flist' <- if Recursive `elem` opts
              then expandDirs origfiles
              else return origfiles
    let flist = nubsort (parlist ++ toFilePath `map` flist')
    -- refuse to add boring files recursively:
    nboring <- if includeBoring opts
               then return darcsdirFilter
               else boringFileFilter
    let fixedOpts = if DryRun `elem` opts then Verbose:opts else opts
    mapM_ (putWarning fixedOpts . text . ((msgSkipping msgs ++ " boring file ")++)) $
      flist \\ nboring flist
    Sealed ps <- fmap unFreeLeft $ addp msgs fixedOpts cur $ nboring flist
    when (nullFL ps && not (null origfiles)) $
        fail "No files were added"
    unless gotDryRun $ addToPending repository ps
  where
    gotDryRun = DryRun `elem` opts
    msgs | gotDryRun = dryRunMessages
         | otherwise = normalMessages

addp :: forall prim . (Patchy prim, PrimPatch prim) => AddMessages -> [DarcsFlag] -> Tree IO -> [FilePath] -> IO (FreeLeft (FL prim))
addp msgs opts cur0 files = do
    (ps, dups) <-
        foldr
            (\f rest cur accPS accDups -> do
                (cur', mp, mdup) <- addp' cur f
                rest cur' (maybeToList mp ++ accPS) (maybeToList mdup ++ accDups))
            (\_ ps dups -> return (reverse ps, dups))
            files
            cur0 [] []
    let uniq_dups = nub dups
        caseMsg =
            if gotAllowCaseOnly then ":"
                else ";\nnote that to ensure portability we don't allow\n" ++
                     "files that differ only in case. Use --case-ok to override this:"
    unless (null dups) $ do
       dupMsg <-
         case uniq_dups of
         [f] ->
           do
           isDir <- doesDirectoryReallyExist f
           if isDir
               then return $
                 "The following directory "++msgIs msgs++" already in the repository"
               else return $
                 "The following file "++msgIs msgs++" already in the repository"
         fs   ->
           do
           areDirs <- mapM doesDirectoryReallyExist fs
           if and areDirs
               then return $
                 "The following directories "++msgAre msgs++" already in the repository"
               else
                 (if or areDirs
                      then return $
                        "The following files and directories " ++
                        msgAre msgs ++ " already in the repository"
                      else return $
                        "The following files " ++ msgAre msgs ++ " already in the repository")
       putWarning opts . text $ dupMsg ++ caseMsg
       mapM_ (putWarning opts . text) uniq_dups
    return $ foldr (joinGap (+>+)) (emptyGap NilFL) ps
 where
  addp' :: Tree IO -> FilePath -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe FilePath)
  addp' cur f = do
    already_has <- (if gotAllowCaseOnly then treeHas else treeHasAnycase) cur f
    isdir <- doesDirectoryReallyExist f
    isfile <- doesFileReallyExist f
    islink <- isFileReallySymlink f
    case (already_has, is_badfilename, isdir, isfile, islink) of
      (True, _, _, _, _) -> return (cur, Nothing, Just f)
      (_, True, _, _, _) ->
        do putWarning opts . text $
             "The filename " ++ f ++ " is invalid under Windows.\nUse --reserved-ok to allow it."
           return add_failure
      (_, _, True, _, _) -> trypatch $ freeGap (adddir f :>: NilFL)
      (_, _, _, True, _) -> trypatch $ freeGap (addfile f :>: NilFL)
      (_, _, _, _, True) -> do putWarning opts . text $ "Sorry, file " ++ f ++
                                  " is a symbolic link, which is unsupported by darcs."
                               return add_failure
      _ -> do putWarning opts . text $ "File "++ f ++" does not exist!"
              return add_failure
      where is_badfilename = not (gotAllowWindowsReserved || WindowsFilePath.isValid f)
            add_failure = (cur, Nothing, Nothing)
            trypatch :: FreeLeft (FL prim) -> IO (Tree IO, Maybe (FreeLeft (FL prim)), Maybe FilePath)
            trypatch p = do Sealed p' <- return $ unFreeLeft p
                            tree <- applyToTree p' cur
                            putVerbose opts . text $ msgAdding msgs++" '"++f++"'"
                            return (tree, Just p, Nothing)
                          `catch` \_ -> do
                            err <- parent_error
                            putWarning opts . text $ msgSkipping msgs ++ " '" ++ f ++ "' ... " ++ err
                            return (cur, Nothing, Nothing)
            parentdir = takeDirectory f
            have_parentdir = treeHasDir cur parentdir
            parent_error = have_parentdir >>= \x -> return $
                             if x then ""
                                  else "couldn't add parent directory '"++parentdir++"' to repository."
  gotAllowCaseOnly = doAllowCaseOnly opts
  gotAllowWindowsReserved = doAllowWindowsReserved opts

data AddMessages =
    AddMessages
    { msgSkipping  :: String
    , msgAdding    :: String
    , msgIs        :: String
    , msgAre       :: String
    }

normalMessages, dryRunMessages :: AddMessages
normalMessages =
    AddMessages
    { msgSkipping  = "Skipping"
    , msgAdding    = "Adding"
    , msgIs        = "is"
    , msgAre       = "are"
    }
dryRunMessages =
    AddMessages
    { msgSkipping  = "Would skip"
    , msgAdding    = "Would add"
    , msgIs        = "would be"
    , msgAre       = "would be"
    }

expandDirs :: [SubPath] -> IO [SubPath]
expandDirs fs = liftM (map (fromJust . simpleSubPath)) (concat `fmap` mapM (expandOne . toPath) fs)

expandOne :: FilePath -> IO [FilePath]
expandOne "" = listFiles
expandOne f = do
  isdir <- doesDirectoryReallyExist f
  if not isdir then return [f]
     else do fs <- withCurrentDirectory f listFiles
             return $ f: map (f </>) fs

getParents :: Tree IO -> [FilePath] -> [FilePath]
getParents cur = map (anchorPath "") . go . map floatPath
  where go fs = filter (isNothing . findTree cur) $ concatMap parents fs

\end{code}
