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

\darcsCommand{revert}
\begin{code}
module Darcs.Commands.Revert ( revert ) where
import System.Exit ( ExitCode(..), exitWith )
import Control.Applicative ( (<$>) )
import Control.Monad ( when )
import Data.List ( sort )
import Data.Maybe ( fromMaybe )

import English (englishNum, This(..), Noun(..))
import Darcs.Flags( diffingOpts )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Commands.WhatsNew ( announceFiles )
import Darcs.Arguments ( DarcsFlag( All, Debug ),
                        ignoretimes, workingRepoDir,
                        allInteractive,
                        fixSubPaths,
                        listRegisteredFiles, umaskOption,
                      )
import Darcs.Utils ( askUser )
import Darcs.RepoPath ( toFilePath )
import Darcs.Repository ( withRepoLock, RepoJob(..), withGutsOf,
                    addToPending,
                    applyToWorking,
                    amInRepository, readRecorded,
                    unrecordedChanges
                  )
import Darcs.Patch ( invert, applyToFilepaths, commute )
import Darcs.Patch.Split ( reversePrimSplitter )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), lengthFL, nullFL, (+>+) )
import Darcs.SelectChanges ( selectChanges, WhichChanges(Last), selectionContextPrim, runSelection )
import Darcs.Patch.TouchesFiles ( chooseTouching )
import Darcs.Commands.Unrevert ( writeUnrevert )
import Darcs.Witnesses.Sealed ( Sealed(..) )

#include "gadts.h"

revertDescription :: String
revertDescription = "Discard unrecorded changes."

revertHelp :: String
revertHelp =
 "The `darcs revert' command discards unrecorded changes the working\n" ++
 "tree.  As with `darcs record', you will be asked which hunks (changes)\n" ++
 "to revert.  The --all switch can be used to avoid such prompting. If\n" ++
 "files or directories are specified, other parts of the working tree\n" ++
 "are not reverted.\n" ++
 "\n" ++
 "In you accidentally reverted something you wanted to keep (for\n" ++
 "example, typing `darcs rev -a' instead of `darcs rec -a'), you can\n" ++
 "immediately run `darcs unrevert' to restore it.  This is only\n" ++
 "guaranteed to work if the repository has not changed since `darcs\n" ++
 "revert' ran.\n"

revert :: DarcsCommand
revert = DarcsCommand {commandProgramName = "darcs",
                       commandName = "revert",
                       commandHelp = revertHelp,
                       commandDescription = revertDescription,
                       commandExtraArgs = -1,
                       commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                       commandCommand = revertCmd,
                       commandPrereq = amInRepository,
                       commandGetArgPossibilities = listRegisteredFiles,
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [ignoretimes, umaskOption],
                       commandBasicOptions = [allInteractive,
                                               workingRepoDir]}

revertCmd :: [DarcsFlag] -> [String] -> IO ()
revertCmd opts args = withRepoLock opts $ RepoJob $ \repository -> do
  files <- if null args then return Nothing
    else Just . sort <$> fixSubPaths opts args
  announceFiles files "Reverting changes in"
  changes <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -}) repository files
  let pre_changed_files = applyToFilepaths (invert changes) (map toFilePath $ fromMaybe [] files)
  rec <- readRecorded repository
  Sealed touching_changes <- return (chooseTouching pre_changed_files changes)
  (case touching_changes of
    NilFL -> putStrLn "There are no changes to revert!"
    _ -> do
      let context = selectionContextPrim "revert" opts (Just reversePrimSplitter) pre_changed_files
      (norevert:>p) <- runSelection (selectChanges Last changes) context
      if nullFL p
       then putStrLn $ "If you don't want to revert after all," ++
                        " that's fine with me!"
       else do
             let theseChanges = englishNum (lengthFL p) . This . Noun $ "change"
             yorn <- if All `elem` opts
                     then return "y"
                     else askUser $ "Do you really want to revert " ++ theseChanges "? "
             case yorn of ('y':_) -> return ()
                          _ -> exitWith $ ExitSuccess
             withGutsOf repository $ do
                 addToPending repository $ invert p
                 when (Debug `elem` opts) $ putStrLn "About to write the unrevert file."
                 case commute (norevert:>p) of
                   Just (p':>_) -> writeUnrevert repository p' rec NilFL
                   Nothing -> writeUnrevert repository (norevert+>+p) rec NilFL
                 when (Debug `elem` opts) $ putStrLn "About to apply to the working directory."
                 _ <- applyToWorking repository opts (invert p) `catch` \e ->
                     fail ("Unable to apply inverse patch!" ++ show e)
                 return ()) :: IO ()
  putStrLn "Finished reverting."
\end{code}

