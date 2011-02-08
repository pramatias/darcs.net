%  Copyright (C) 2007 Florian Weimer
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

\darcsCommand{show tags}
\begin{code}
module Darcs.Commands.ShowTags ( showTags ) where
import Darcs.Arguments ( DarcsFlag(..), possiblyRemoteRepoDir, getRepourl )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Patch.PatchInfoAnd ( info )
import Darcs.Repository ( findRepository, readRepo, withRepositoryDirectory, RepoJob(..) )
import Darcs.Patch.Info ( piTag )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Witnesses.Ordered ( mapRL )
import Data.Maybe ( fromMaybe )
import System.IO ( stderr, hPutStrLn )
-- import Printer ( renderPS )

showTagsDescription :: String
showTagsDescription = "Show all tags in the repository."

showTagsHelp :: String
showTagsHelp =
 "The tags command writes a list of all tags in the repository to standard\n"++
 "output.\n" ++
 "\n" ++
 "Tab characters (ASCII character 9) in tag names are changed to spaces\n" ++
 "for better interoperability with shell tools.  A warning is printed if\n" ++
 "this happens."

showTags :: DarcsCommand
showTags = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "tags",
  commandHelp = showTagsHelp,
  commandDescription = showTagsDescription,
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = tagsCmd,
  commandPrereq = findRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [possiblyRemoteRepoDir] }

tagsCmd :: [DarcsFlag] -> [String] -> IO ()
tagsCmd opts _ =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryDirectory opts repodir $ RepoJob $ \repository -> do
  patches <- readRepo repository
  sequence_ $ mapRL process $ newset2RL patches
  where process hp =
            case piTag $ info hp of
              Just t -> do
                 t' <- normalize t t False
                 putStrLn t'
              Nothing -> return ()
        normalize :: String -> String -> Bool -> IO String
        normalize _ [] _ = return []
        normalize t (x : xs) flag =
            if x == '\t' then do
                  if flag
                    then return ()
                    else hPutStrLn stderr
                             ("warning: tag with TAB character: " ++ t)
                  rest <- (normalize t xs True)
                  return $ ' ' : rest
            else do
                  rest <- (normalize t xs flag)
                  return $ x : rest

\end{code}
