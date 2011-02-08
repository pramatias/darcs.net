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

\darcsCommand{check}
\begin{code}
module Darcs.Commands.Check ( check ) where
import Control.Monad ( when )
import Control.Applicative( (<$>) )
import System.Exit ( ExitCode(..), exitWith )



import Darcs.Commands ( DarcsCommand(..), nodefaults, putInfo )
import Darcs.Arguments ( DarcsFlag(Quiet),
                         test,
                        leaveTestDir, workingRepoDir, ignoretimes
                      )
import Darcs.Flags(willIgnoreTimes)
import Darcs.Repository.Repair( replayRepository, checkIndex
                              , RepositoryConsistency(..) )
import Darcs.Repository ( Repository, amInRepository, withRepository,
                          testRecorded, readRecorded, RepoJob(..) )
import Darcs.Patch ( RepoPatch, showPatch, PrimOf )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Diff( treeDiff )
import Printer ( text, ($$), (<+>) )

#include "gadts.h"

checkDescription :: String
checkDescription = "Check the repository for consistency."

checkHelp :: String
checkHelp =
 "This command verifies that the patches in the repository, when applied\n" ++
 "successively to an empty tree, result in the pristine tree.  If not,\n" ++
 "the differences are printed and Darcs exits unsucessfully (with a\n" ++
 "non-zero exit status).\n" ++
 "\n" ++
 "If a regression test is defined (see `darcs setpref') it will be run\n" ++
 "by `darcs check'.  Use the --no-test option to disable this.\n"

check :: DarcsCommand
check = DarcsCommand {commandProgramName = "darcs",
                      commandName = "check",
                      commandHelp = checkHelp,
                      commandDescription = checkDescription,
                      commandExtraArgs = 0,
                      commandExtraArgHelp = [],
                      commandCommand = checkCmd,
                      commandPrereq = amInRepository,
                      commandGetArgPossibilities = return [],
                      commandArgdefaults = nodefaults,
                      commandAdvancedOptions = [],
                      commandBasicOptions = [ test,
                                              leaveTestDir,
                                              workingRepoDir,
                                               ignoretimes
                                             ]}

checkCmd :: [DarcsFlag] -> [String] -> IO ()
checkCmd opts _ = withRepository opts (RepoJob (check' opts))

check'
  :: forall p C(r u t) . (RepoPatch p) => [DarcsFlag] -> Repository p C(r u t) -> IO ()
check' opts repository = do
    failed <- replayRepository repository opts $ \ state -> do
      case state of
        RepositoryConsistent -> do
          putInfo opts $ text "The repository is consistent!"
          rc <- testRecorded repository
          when (rc /= ExitSuccess) $ exitWith rc
          return False
        BrokenPristine newpris -> do
          brokenPristine newpris
          return True
        BrokenPatches newpris _ -> do
          brokenPristine newpris
          putInfo opts $ text "Found broken patches."
          return True
    bad_index <- case willIgnoreTimes opts of
                   False -> not <$> checkIndex repository (Quiet `elem` opts)
                   True -> return False
    when bad_index $ putInfo opts $ text "Bad index."
    exitWith $ if failed || bad_index then ExitFailure 1 else ExitSuccess
   where
     brokenPristine newpris = do
         putInfo opts $ text "Looks like we have a difference..."
         mc <- readRecorded repository
         ftf <- filetypeFunction
         Sealed (diff :: FL (PrimOf p) C(r r2)) <- unFreeLeft `fmap` treeDiff ftf newpris mc :: IO (Sealed (FL (PrimOf p) C(r)))
         putInfo opts $ case diff of
                        NilFL -> text "Nothing"
                        patch -> text "Difference: " <+> showPatch patch
         putInfo opts $ text ""
                     $$ text "Inconsistent repository!"

\end{code}
%% FIXME: this should go in "common options" or something, since
%% commands like record and amend-record also run the test command.
\input{Darcs/Test.lhs}
