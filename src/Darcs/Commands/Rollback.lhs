%  Copyright (C) 2002-2004,2007 David Roundy
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

\darcsCommand{rollback}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Rollback ( rollback ) where

import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode(..) )
import Data.List ( sort )
import Data.Maybe ( fromMaybe, isJust )
import System.Directory ( removeFile )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag(MarkConflicts), fixSubPaths, getAuthor,
                         setEnvDarcsPatches,
                         workingRepoDir, nocompress,
                         author, patchnameOption, askLongComment,
                         leaveTestDir, notest, listRegisteredFiles,
                         matchSeveralOrLast, allInteractive, umaskOption,
                         recordRollback
                       )
import Darcs.RepoPath ( toFilePath )
import Darcs.Repository ( Repository, amInRepository, withRepoLock, RepoJob(..),
                          applyToWorking,
                          readRepo,
                          tentativelyMergePatches, withGutsOf,
                          testTentative,
                          finalizeRepositoryChanges, invalidateIndex,
                          tentativelyAddToPending, considerMergeToWorking
                        )
import Darcs.Patch ( RepoPatch, summary, invert, namepatch, effect, fromPrims,
                     sortCoalesceFL, canonize, anonymous, PrimOf )
import Darcs.Flags ( isInteractive, rollbackInWorkingDir )
import Darcs.Patch.Set ( PatchSet(..), newset2FL )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Witnesses.Ordered
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia )
import Darcs.Lock ( worldReadableTemp )
import Darcs.Match ( firstMatch )
import Darcs.SelectChanges ( selectChanges,
                             WhichChanges(..),
                             selectionContext, selectionContextPrim,
                             runSelection
                           )
import Darcs.Commands.Record ( getLog )
import Darcs.Commands.Unrecord ( getLastPatches )
import Darcs.Commands.WhatsNew ( announceFiles, filterExistingFiles )
import Darcs.Utils ( clarifyErrors, PromptConfig(..), promptChar )
import Printer ( renderString )
import Progress ( debugMessage )
import Darcs.Witnesses.Sealed ( Sealed(..) )
import IsoDate ( getIsoDateTime )
#include "impossible.h"
#include "gadts.h"

rollbackDescription :: String
rollbackDescription =
 "Record a new patch reversing some recorded changes."

rollbackHelp :: String
rollbackHelp =
 "Rollback is used to undo the effects of one or more patches without actually\n"++
 "deleting them.  Instead, it creates a new patch reversing selected portions.\n"++
 "of those changes. Unlike obliterate and unrecord (which accomplish a similar\n"++
 "goal) rollback is perfectly safe, since it leaves in the repository a record\n"++
 "of its changes.\n"

rollback :: DarcsCommand
rollback = DarcsCommand {commandProgramName = "darcs",
                         commandName = "rollback",
                         commandHelp = rollbackHelp,
                         commandDescription = rollbackDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = rollbackCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [nocompress,umaskOption],
                         commandBasicOptions = [matchSeveralOrLast,
                                                  allInteractive,
                                                  author, patchnameOption, askLongComment,
                                                  notest, leaveTestDir,
                                                  workingRepoDir, recordRollback]}

rollbackCmd :: [DarcsFlag] -> [String] -> IO ()
rollbackCmd opts args = withRepoLock opts $ RepoJob $ \repository -> do
  files <- if null args then return Nothing
    else Just . sort <$> fixSubPaths opts args
  when (files == Just []) $ fail "No valid arguments were given."
  let files_fp = map toFilePath $ fromMaybe [] files
  announceFiles files "Recording changes in"
  existing_files <- maybe (return Nothing)
    (fmap Just . filterExistingFiles repository) files
  when (existing_files == Just []) $
       fail "None of the files you specified exist!"
  allpatches <- readRepo repository
  (_ :> patches) <- return $ if firstMatch opts
                             then getLastPatches opts allpatches
                             else (PatchSet NilRL NilRL):> (newset2FL allpatches)
  let patches_context = selectionContext "rollback" opts Nothing files_fp
  (_ :> ps) <- runSelection (selectChanges LastReversed patches) patches_context
  when (nullFL ps) $ do putStrLn "No patches selected!"
                        exitWith ExitSuccess
  setEnvDarcsPatches ps
  let hunks_context = selectionContextPrim "rollback" opts (Just reversePrimSplitter) files_fp
      hunks = (concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect ps)
  runSelection (selectChanges Last hunks) hunks_context >>=
               if (rollbackInWorkingDir opts)
               then (undoItNow opts repository)
               else (rollItBackNow opts repository ps)

rollItBackNow :: (RepoPatch p) =>
                [DarcsFlag] -> Repository p C(r u t) ->  FL (PatchInfoAnd p) C(x y)
                            -> (q :> FL (PrimOf p)) C(a t) -> IO ()
rollItBackNow opts repository  ps (_ :> ps'') =
         do when (nullFL ps'') $ do putStrLn "No changes selected!"
                                    exitWith ExitSuccess
            let make_log = worldReadableTemp "darcs-rollback"
                newlog = Just ("", "":"rolling back:":"":lines (renderString $ summary ps ))
            --tentativelyRemovePatches repository opts (mapFL_FL hopefully ps)
            (name, my_log, logf) <- getLog opts newlog make_log $ invert ps''
            date <- getIsoDateTime
            my_author <- getAuthor opts
            rbp <- n2pia `fmap` namepatch date name my_author my_log
                                          (fromPrims $ invert ps'')
            debugMessage "Adding rollback patch to repository."
            Sealed pw <-
                tentativelyMergePatches repository "rollback"
                                            (MarkConflicts : opts) NilFL
                                            (rbp :>: NilFL)
            debugMessage "Finalizing rollback changes..."
            invalidateIndex repository
            rc <- testTentative repository
            when (rc /= ExitSuccess) $ do
                when (not $ isInteractive opts) $ exitWith rc
                putStrLn $ "Looks like you have a bad patch: '"++name++"'"
                let prompt = "Shall I rollback anyway?"
                yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
                case yn of
                  'y' -> return ()
                  _ -> exitWith rc
            withGutsOf repository $ do
              finalizeRepositoryChanges repository
              debugMessage "About to apply rolled-back changes to working directory."
              _ <- revertable $ applyToWorking repository opts pw
              return ()
            when (isJust logf) $ removeFile (fromJust logf)
            putStrLn "Finished rolling back."
          where revertable x = x `clarifyErrors` unlines
                  ["Error applying patch to the working directory.","",
                   "This may have left your working directory an inconsistent",
                   "but recoverable state. If you had no un-recorded changes",
                   "by using 'darcs revert' you should be able to make your",
                   "working directory consistent again."]

undoItNow :: (RepoPatch p) => [DarcsFlag] -> Repository p C(r u t)
          -> (q :> FL (PrimOf p)) C(a t) -> IO ()
undoItNow opts repo (_ :> prims) =
    do
      rbp <- n2pia `fmap` anonymous (fromPrims $ invert prims)
      Sealed pw <- considerMergeToWorking repo "rollback" (MarkConflicts: opts)
                                                     NilFL (rbp :>: NilFL)
      tentativelyAddToPending repo opts pw
      withGutsOf repo $ do
        finalizeRepositoryChanges repo
        _ <- applyToWorking repo opts pw `catch` \e ->
            fail ("error applying rolled back patch to working directory\n"
                  ++ show e)
        debugMessage "Finished applying unrocorded rollback patch"
\end{code}

