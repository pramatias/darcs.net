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

\darcsCommand{remove}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Remove ( remove, rm, unadd ) where

import Control.Monad ( when, foldM )
import Darcs.Commands ( DarcsCommand(..), nodefaults,
                        commandAlias, commandStub,
                        putWarning
                      )
import Darcs.Arguments ( DarcsFlag (Recursive), fixSubPaths,
                        listRegisteredFiles,
                        workingRepoDir, umaskOption,
                        recursive
                      )
import Darcs.RepoPath ( SubPath, sp2fn )
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..), amInRepository,
                          addToPending, readRecordedAndPending, readUnrecorded )
import Darcs.Diff( treeDiff )
import Darcs.Patch ( RepoPatch, PrimOf, PrimPatch, adddir, rmdir, addfile, rmfile )
import Darcs.Patch.FileName( fn2fp )
import Darcs.Witnesses.Ordered ( FL(..), (+>+) )
import Darcs.Witnesses.Sealed ( Sealed(..), Gap(..), FreeLeft, unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction, FileType )
import Storage.Hashed.Tree( Tree, TreeItem(..), find, modifyTree, expand, list )
import Storage.Hashed.AnchoredPath( anchorPath, AnchoredPath )
import Storage.Hashed( floatPath )

import Darcs.Commands.Add( expandDirs )
import Printer ( text )

#include "gadts.h"

removeDescription :: String
removeDescription = "Remove files from version control."

removeHelp :: String
removeHelp =
 "The `darcs remove' command exists primarily for symmetry with `darcs\n" ++
 "add', as the normal way to remove a file from version control is\n" ++
 "simply to delete it from the working tree.  This command is only\n" ++
 "useful in the unusual case where one wants to record a removal patch\n" ++
 "WITHOUT deleting the copy in the working tree (which can be re-added).\n" ++
 "\n" ++
 "Note that applying a removal patch to a repository (e.g. by pulling\n" ++
 "the patch) will ALWAYS affect the working tree of that repository.\n"

remove :: DarcsCommand
remove = DarcsCommand {commandProgramName = "darcs",
                       commandName = "remove",
                       commandHelp = removeHelp,
                       commandDescription = removeDescription,
                       commandExtraArgs = -1,
                       commandExtraArgHelp = ["<FILE or DIRECTORY> ..."],
                       commandCommand = removeCmd,
                       commandPrereq = amInRepository,
                       commandGetArgPossibilities = listRegisteredFiles,
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [umaskOption],
                       commandBasicOptions =
                           [workingRepoDir, recursive "recurse into subdirectories"]}

removeCmd :: [DarcsFlag] -> [String] -> IO ()
removeCmd opts relargs = do
    when (null relargs) $
        fail "Nothing specified, nothing removed."
    origfiles <- fixSubPaths opts relargs
    when (null origfiles) $
        fail "No valid arguments were given."
    withRepoLock opts $ RepoJob $ \repository -> do
    args <- if Recursive `elem` opts
            then reverse `fmap` expandDirs origfiles
            else return origfiles
    Sealed p <- makeRemovePatch opts repository args
    addToPending repository p

-- | makeRemovePatch builds a list of patches to remove the given filepaths.
--   This function does not recursively process directories. The 'Recursive'
--   flag should be handled by the caller by adding all offspring of a directory
--   to the files list.
makeRemovePatch :: RepoPatch p => [DarcsFlag] -> Repository p C(r u t)
                  -> [SubPath] -> IO (Sealed (FL (PrimOf p) C(u)))
makeRemovePatch opts repository files =
                          do recorded <- expand =<< readRecordedAndPending repository
                             unrecorded <- readUnrecorded repository $ Just files
                             ftf <- filetypeFunction
                             result <- foldM removeOnePath (ftf,recorded,unrecorded, []) $ map (floatPath . fn2fp . sp2fn) files
                             case result of
                                 (_, _, _, patches) -> return $
                                                         unFreeLeft $ foldr (joinGap (+>+)) (emptyGap NilFL) $ reverse patches
    where removeOnePath (ftf, recorded, unrecorded, patches) f = do
            let recorded' = modifyTree recorded f Nothing
                unrecorded' = modifyTree unrecorded f Nothing
            local <- makeRemoveGap opts ftf recorded unrecorded unrecorded' f
            -- we can tell if the remove succeeded by looking if local is
            -- empty. If the remove succeeded, we should pass on updated
            -- recorded and unrecorded that reflect the removal
            return $ case local of
                       Just gap -> (ftf, recorded', unrecorded', gap : patches)
                       _        -> (ftf, recorded, unrecorded, patches)

-- | Takes a file path and returns the FL of patches to remove that, wrapped in
--   a 'Gap'.
--   Returns 'Nothing' in case the path cannot be removed (if it is not tracked,
--   or if it's a directory and it's not tracked).
--   The three 'Tree' arguments are the recorded state, the unrecorded state
--   excluding the removal of this file, and the unrecorded state including the
--   removal of this file.
makeRemoveGap :: PrimPatch prim => [DarcsFlag] -> (FilePath -> FileType)
                -> Tree IO -> Tree IO -> Tree IO -> AnchoredPath
                -> IO (Maybe (FreeLeft (FL prim)))
makeRemoveGap opts ftf recorded unrecorded unrecorded' f =
    case (find recorded f, find unrecorded f) of
        (Just (SubTree _), Just (SubTree unrecordedChildren)) -> do
            if not $ null (list unrecordedChildren)
              then skipAndWarn "it is not empty"
              else return $ Just $ freeGap (rmdir f_fp :>: NilFL)
        (Just (File _), Just (File _)) ->
            Just `fmap` treeDiff ftf unrecorded unrecorded'
        (Just (File _), _) ->
            return $ Just $ freeGap (addfile f_fp :>: rmfile f_fp :>: NilFL)
        (Just (SubTree _), _) ->
            return  $ Just $ freeGap (adddir f_fp :>: rmdir f_fp :>: NilFL)
        (_, _) -> skipAndWarn "it is not tracked by darcs"
  where f_fp = anchorPath "" f
        skipAndWarn reason =
            do putWarning opts . text $ "Can't remove " ++ f_fp
                                        ++ " (" ++ reason ++ ")"
               return $ Nothing


rmDescription :: String
rmDescription = "Help newbies find `darcs remove'."

rmHelp :: String
rmHelp =
 "The `darcs rm' command does nothing.\n" ++
 "\n" ++
 "The normal way to remove a file from version control is simply to\n" ++
 "delete it from the working tree.  To remove a file from version\n" ++
 "control WITHOUT affecting the working tree, see `darcs remove'.\n"

rm :: DarcsCommand
rm = commandStub "rm" rmHelp rmDescription remove

unadd :: DarcsCommand
unadd = commandAlias "unadd" Nothing remove
\end{code}


