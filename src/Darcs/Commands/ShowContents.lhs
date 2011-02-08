%  Copyright (C) 2007 Eric Kow
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

\darcsCommand{show contents}
\begin{code}
module Darcs.Commands.ShowContents ( showContents ) where

import Control.Monad ( filterM, forM_, unless )
import Control.Monad.Trans( liftIO )
import System.IO ( stdout )
import Data.Maybe( fromJust )

import qualified Data.ByteString as B

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag, matchOne,
                         workingRepoDir, fixSubPaths )
import Darcs.RepoPath ( sp2fn )
import Darcs.IO ( mReadFilePS, mDoesFileExist )
import Darcs.Patch.Match( Matcher )
import Darcs.Match ( haveNonrangeMatch, applyInvToMatcher, nonrangeMatcher
                   , InclusiveOrExclusive(..), matchExists )
import Darcs.Repository ( withRepository, RepoJob(..), findRepository, readRepo, readRecorded )
import Darcs.Patch( RepoPatch )
import Storage.Hashed.Monad( virtualTreeIO )

showContentsDescription :: String
showContentsDescription = "Outputs a specific version of a file."

showContentsHelp :: String
showContentsHelp =
  "Show contents can be used to display an earlier version of some file(s).\n"++
  "If you give show contents no version arguments, it displays the recorded\n"++
  "version of the file(s).\n"

showContents :: DarcsCommand
showContents = DarcsCommand {commandProgramName = "darcs",
                              commandName = "contents",
                              commandHelp = showContentsHelp,
                              commandDescription = showContentsDescription,
                              commandExtraArgs = -1,
                              commandExtraArgHelp
                                    = ["[FILE]..."],
                              commandCommand = showContentsCmd,
                              commandPrereq = findRepository,
                              commandGetArgPossibilities = return [],
                              commandArgdefaults = nodefaults,
                              commandAdvancedOptions = [],
                              commandBasicOptions = [matchOne, workingRepoDir]}

getMatcher :: (RepoPatch p) => [DarcsFlag] -> Matcher p
getMatcher = fromJust . nonrangeMatcher

showContentsCmd :: [DarcsFlag] -> [String] -> IO ()
showContentsCmd _ [] = fail
 "show contents needs at least one argument."
showContentsCmd opts args = withRepository opts $ RepoJob $ \repository -> do
  path_list <- map sp2fn `fmap` fixSubPaths opts args
  pristine <- readRecorded repository
  let matcher = getMatcher opts
      unapply_to_match = applyInvToMatcher Exclusive matcher
  matched <- if (haveNonrangeMatch opts)
                 then do patchset <- readRepo repository
                         unless (matchExists matcher patchset) $
                                fail $ "Couldn't match pattern " ++ show matcher
                         snd `fmap` virtualTreeIO (unapply_to_match patchset) pristine
                 else return pristine
  let dump = do okpaths <- filterM mDoesFileExist path_list
                forM_ okpaths $ \f -> do content <- mReadFilePS f
                                         liftIO (B.hPut stdout content)
-- Warning:  A do-notation statement discarded a result of type ((), Storage.Hashed.Tree.Tree IO).
  _ <- virtualTreeIO dump matched
  return ()
\end{code}
