%  Copyright (C) 2002-2003 David Roundy
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

\darcsCommand{move}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Move ( move, mv ) where
import Control.Applicative ( (<$>) )
import Control.Monad ( when, unless, zipWithM_ )
import Data.Maybe ( catMaybes )
import Darcs.SignalHandler ( withSignalsBlocked )

import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias )
import Darcs.Arguments ( DarcsFlag(), maybeFixSubPaths,
                         fixSubPaths, workingRepoDir,
                        listFiles, allowProblematicFilenames, umaskOption,
                      )
import Darcs.Flags ( doAllowCaseOnly, doAllowWindowsReserved )
import Darcs.RepoPath ( SubPath(), toFilePath )
import System.FilePath.Posix ( (</>), takeFileName )
import System.Directory ( renameDirectory )
import Workaround ( renameFile )
import Darcs.Repository.State ( readRecordedAndPending )
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..), amInRepository, addToPending )
import Darcs.Witnesses.Ordered ( FL(..), toFL )
import Darcs.Witnesses.Sealed ( Sealed(..), unseal, freeGap, FreeLeft, unFreeLeft )
import Darcs.Global ( debugMessage )
import qualified Darcs.Patch
import Darcs.Patch ( RepoPatch, PrimPatch )
import Darcs.Patch.FileName ( fp2fn, fn2fp, superName )
import Data.List ( nub, sort )
import qualified System.FilePath.Windows as WindowsFilePath

import Darcs.Utils( treeHas, treeHasDir, treeHasAnycase, treeHasFile )
import Storage.Hashed.Tree( Tree, modifyTree )
import Storage.Hashed.Plain( readPlainTree )
import Storage.Hashed.AnchoredPath( floatPath )

#include "gadts.h"

moveDescription :: String
moveDescription = "Move or rename files."

moveHelp :: String
moveHelp =
 "Darcs cannot reliably distinguish between a file being deleted and a\n" ++
 "new one added, and a file being moved.  Therefore Darcs always assumes\n" ++
 "the former, and provides the `darcs mv' command to let Darcs know when\n" ++
 "you want the latter.  This command will also move the file in the\n" ++
 "working tree (unlike `darcs remove'), unless it has already been moved.\n" ++
 "\n" ++
 -- Note that this paragraph is very similar to one in ./Add.lhs.
 "Darcs will not rename a file if another file in the same folder has\n" ++
 "the same name, except for case.  The --case-ok option overrides this\n" ++
 "behaviour.  Windows and OS X usually use filesystems that do not allow\n" ++
 "files a folder to have the same name except for case (for example,\n" ++
 "`ReadMe' and `README').  If --case-ok is used, the repository might be\n" ++
 "unusable on those systems!\n"

move :: DarcsCommand
move = DarcsCommand {commandProgramName = "darcs",
                   commandName = "move",
                   commandHelp = moveHelp,
                   commandDescription = moveDescription,
                   commandExtraArgs = -1,
                   commandExtraArgHelp = ["<SOURCE> ... <DESTINATION>"],
                   commandCommand = moveCmd,
                   commandPrereq = amInRepository,
                   commandGetArgPossibilities = listFiles,
                   commandArgdefaults = nodefaults,
                   commandAdvancedOptions = [umaskOption],
                   commandBasicOptions = [allowProblematicFilenames, workingRepoDir]}

moveCmd :: [DarcsFlag] -> [String] -> IO ()
moveCmd opts args
  | length args < 2 = fail $ "The `darcs move' command requires at least" ++
      "two arguments."
  | length args == 2 = do
      xs <- maybeFixSubPaths opts args
      case xs of
        [Just from, Just to]
          | from == to -> fail "Cannot rename a file or directory onto itself!"
          | otherwise -> moveFile opts from to
        _ -> fail "Both source and destination must be valid."
  | otherwise = let (froms, to) = (init args, last args) in do
      x <- head <$> maybeFixSubPaths opts [to]
      case x of
        Nothing -> fail "Invalid destination directory."
        Just to' -> do
          xs <- nub . sort <$> fixSubPaths opts froms
          case xs of
            [] -> fail "Nothing to move."
            froms' -> moveFilesToDir opts froms' to'

moveFile :: [DarcsFlag] -> SubPath -> SubPath -> IO ()
moveFile opts old new = withRepoLock opts $ RepoJob $ \repository -> do
  work <- readPlainTree "."
  let old_fp = toFilePath old
      new_fp = toFilePath new
  has_new <- treeHasDir work new_fp
  has_old <- treeHas work old_fp
  if has_new && has_old
   then moveToDir repository opts [old_fp] new_fp
   else do
    cur <- readRecordedAndPending repository
    addpatch <- checkNewAndOldFilenames opts cur work (old_fp,new_fp)
    withSignalsBlocked $ do
      case unFreeLeft <$> addpatch of
        Nothing -> addToPending repository (Darcs.Patch.move old_fp new_fp :>: NilFL)
        Just (Sealed p) -> addToPending repository (p :>: Darcs.Patch.move old_fp new_fp :>: NilFL)
      moveFileOrDir work old_fp new_fp

moveFilesToDir :: [DarcsFlag] -> [SubPath] -> SubPath -> IO ()
moveFilesToDir opts froms to = withRepoLock opts $ RepoJob $ \repo ->
  moveToDir repo opts (map toFilePath froms) $ toFilePath to

moveToDir :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> [FilePath] -> FilePath -> IO ()
moveToDir repository opts moved finaldir =
  let movefns = map takeFileName moved
      movetargets = map (finaldir </>) movefns
      movepatches = zipWith (\a b -> freeGap (Darcs.Patch.move a b)) moved movetargets
  in do
    cur <- readRecordedAndPending repository
    work <- readPlainTree "."
    addpatches <- mapM (checkNewAndOldFilenames opts cur work) $ zip moved movetargets
    withSignalsBlocked $ do
      unseal (addToPending repository) $ toFL $ catMaybes addpatches ++ movepatches
      zipWithM_ (moveFileOrDir work) moved movetargets

checkNewAndOldFilenames
    :: PrimPatch prim => [DarcsFlag] -> Tree IO -> Tree IO -> (FilePath, FilePath) -> IO (Maybe (FreeLeft prim))
checkNewAndOldFilenames opts cur work (old,new) = do
  unless (doAllowWindowsReserved opts || WindowsFilePath.isValid new) $
     fail $ "The filename " ++ new ++ " is not valid under Windows.\n" ++
            "Use --reserved-ok to allow such filenames."
  has_work <- treeHas work old
  has_cur <- treeHas cur old
  maybe_add_file_thats_been_moved <-
     if has_work -- We need to move the object
     then do has_target <- treeHasDir work (fn2fp $ superName $ fp2fn new)
             unless has_target $
                    fail $ "The target directory " ++
                             (fn2fp $ superName $ fp2fn new)++
                             " isn't known in working directory, did you forget to add it?"
             has_new <- it_has work
             when has_new $ fail $ already_exists "working directory"
             return Nothing
     else do has_new <- treeHas work new
             unless has_new $ fail $ doesnt_exist "working directory"
             return (Just (freeGap (Darcs.Patch.addfile old)))
  if has_cur
     then do has_target <- treeHasDir cur (fn2fp $ superName $ fp2fn new)
             unless has_target $
                    fail $ "The target directory " ++
                             (fn2fp $ superName $ fp2fn new)++
                             " isn't known in working directory, did you forget to add it?"
             has_new <- it_has cur
             when has_new $ fail $ already_exists "repository"
     else fail $ doesnt_exist "repository"
  return maybe_add_file_thats_been_moved
    where it_has s = treeHas_case (modifyTree s (floatPath old) Nothing) new
          treeHas_case = if doAllowCaseOnly opts then treeHas else treeHasAnycase
          already_exists what_slurpy =
              if doAllowCaseOnly opts
              then "A file or dir named "++new++" already exists in "
                       ++ what_slurpy ++ "."
              else "A file or dir named "++new++" (or perhaps differing"++
                       " only in case)\nalready exists in "++
                       what_slurpy ++ ".\n"++
                   "Use --case-ok to allow files differing only in case."
          doesnt_exist what_slurpy =
              "There is no file or dir named " ++ old ++
              " in the "++ what_slurpy ++ "."

moveFileOrDir :: Tree IO -> FilePath -> FilePath -> IO ()
moveFileOrDir work old new = do
  has_file <- treeHasFile work old
  has_dir <- treeHasDir work old
  when has_file $ do debugMessage $ unwords ["renameFile",old,new]
                     renameFile old new
  when has_dir $ do debugMessage $ unwords ["renameDirectory",old,new]
                    renameDirectory old new

mv :: DarcsCommand
mv = commandAlias "mv" Nothing move
\end{code}


