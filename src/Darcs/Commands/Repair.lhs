%  Copyright (C) 2003,2005 David Roundy
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

\darcsCommand{repair}
\begin{code}
module Darcs.Commands.Repair ( repair, repairCmd ) where
import Control.Monad( unless )
import System.Directory( renameFile )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag( Quiet ),
                        workingRepoDir, umaskOption,
                      )
import Darcs.Repository ( withRepoLock, RepoJob(..), amInRepository,
                          replacePristine, writePatchSet )
import Darcs.Repository.Repair( replayRepository, checkIndex
                              , RepositoryConsistency(..) )

repairDescription :: String
repairDescription = "Repair a corrupted repository."

repairHelp :: String
repairHelp =
 "The `darcs repair' command attempts to fix corruption in the current\n" ++
 "repository.  Currently it can only repair damage to the pristine tree,\n" ++
 "which is where most corruption occurs.\n"

repair :: DarcsCommand
repair = DarcsCommand {commandProgramName = "darcs",
                       commandName = "repair",
                       commandHelp = repairHelp,
                       commandDescription = repairDescription,
                       commandExtraArgs = 0,
                       commandExtraArgHelp = [],
                       commandCommand = repairCmd,
                       commandPrereq = amInRepository,
                       commandGetArgPossibilities = return [],
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [umaskOption],
                       commandBasicOptions = [workingRepoDir]}

repairCmd :: [DarcsFlag] -> [String] -> IO ()
repairCmd opts _ = withRepoLock opts $ RepoJob $ \repository -> do
  replayRepository repository opts $ \state ->
    case state of
      RepositoryConsistent ->
          putStrLn "The repository is already consistent, no changes made."
      BrokenPristine tree -> do
               putStrLn "Fixing pristine tree..."
               replacePristine repository tree
      BrokenPatches tree newps  -> do
               putStrLn "Writing out repaired patches..."
-- Warning:  A do-notation statement discarded a result of type Darcs.Repository.InternalTypes.Repository p ghc-prim
               _ <- writePatchSet newps opts
               putStrLn "Fixing pristine tree..."
               replacePristine repository tree
               return ()
  index_ok <- checkIndex repository (Quiet `elem` opts)
  unless index_ok $ do renameFile "_darcs/index" "_darcs/index.bad"
                       putStrLn "Bad index discarded."

\end{code}
