%  Copyright (C) 2005 Florian Weimer
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

\darcsCommand{show files}
\begin{code}
{-# LANGUAGE CPP #-}
#include "gadts.h"
module Darcs.Commands.ShowFiles ( showFiles
                                , manifestCmd, toListManifest -- for alias
                                ) where
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir,
                        files, directories, pending, nullFlag, matchOne )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Repository ( Repository, amInRepository, withRepository,
                          RepoJob(..) )
import Darcs.Patch ( RepoPatch )
import Darcs.Repository.State ( readRecorded, readRecordedAndPending )
import Storage.Hashed.Tree( Tree, TreeItem(..), list, expand )
import Storage.Hashed.AnchoredPath( anchorPath )
import Storage.Hashed.Plain( readPlainTree )

import Data.List( isPrefixOf )

import Darcs.Match ( haveNonrangeMatch, getNonrangeMatch )
import Darcs.Lock ( withDelayedDir )
showFilesDescription :: String
showFilesDescription = "Show version-controlled files in the working copy."

showFilesHelp :: String
showFilesHelp =
 "The `darcs show files' command lists those files and directories in\n" ++
 "the working tree that are under version control.  This command is\n" ++
 "primarily for scripting purposes; end users will probably want `darcs\n" ++
 "whatsnew --summary'.\n" ++
 "\n" ++
 "A file is `pending' if it has been added but not recorded.  By\n" ++
 "default, pending files (and directories) are listed; the --no-pending\n" ++
 "option prevents this.\n" ++
 "\n" ++
 "By default `darcs show files' lists both files and directories, but\n" ++
 "the alias `darcs show manifest' only lists files.  The --files,\n" ++
 "--directories, --no-files and --no-directories modify this behaviour.\n" ++
 "\n" ++
 "By default entries are one-per-line (i.e. newline separated).  This\n" ++
 "can cause problems if the files themselves contain newlines or other\n" ++
 "control characters.  To get aroudn this, the --null option uses the\n" ++
 "null character instead.  The script interpreting output from this\n" ++
 "command needs to understand this idiom; `xargs -0' is such a command.\n" ++
 "\n" ++
 "For example, to list version-controlled files by size:\n" ++
 "\n" ++
 "    darcs show files -0 | xargs -0 ls -ldS\n"

showFiles :: DarcsCommand
showFiles = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "files",
  commandHelp = showFilesHelp,
  commandDescription = showFilesDescription,
  commandExtraArgs = -1,
  commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
  commandCommand = manifestCmd toListFiles,
  commandPrereq = amInRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, pending, nullFlag, matchOne,
                          workingRepoDir] }

toListFiles, toListManifest :: [DarcsFlag] -> Tree m -> [FilePath]
toListFiles    opts = filesDirs (NoFiles `notElem` opts) (NoDirectories `notElem` opts)
toListManifest opts = filesDirs (NoFiles `notElem` opts) (Directories `elem` opts)

filesDirs :: Bool -> Bool -> Tree m -> [FilePath]
filesDirs False False _ = []
filesDirs False True  t = "." : [ anchorPath "." p | (p, SubTree _) <- list t ]
filesDirs True  False t = [ anchorPath "." p | (p, File _) <- list t ]
filesDirs True  True  t = "." : (map (anchorPath "." . fst) $ list t)

manifestCmd :: ([DarcsFlag] -> Tree IO -> [FilePath]) -> [DarcsFlag] -> [String] -> IO ()
manifestCmd to_list opts argList = do
    list' <- (to_list opts) `fmap` withRepository opts (RepoJob myslurp)
    case argList of
        [] -> mapM_ output list'
        prefixes -> mapM_ output (onlysubdirs prefixes list')
    where myslurp :: RepoPatch p => Repository p C(r u r) -> IO (Tree IO)
          myslurp r = do let fRevisioned = haveNonrangeMatch opts
                             fPending = Pending `elem` opts
                             fNoPending = NoPending `elem` opts
                       -- this covers all 8 options
                         expand =<< case (fRevisioned,fPending,fNoPending) of
                            (True,False,_) -> slurpRevision opts r
                            (True,True,_) -> error $ "can't mix revisioned and pending flags"
                            (False,False,True) -> readRecorded r
                            (False,_,False) -> readRecordedAndPending r -- pending is default
                            (False,True,True) -> error $ "can't mix pending and no-pending flags"
          output_null name = do { putStr name ; putChar '\0' }
          output = if NullFlag `elem` opts then output_null else putStrLn
          isParentDir a b = a == b
                            || (a  ++ "/") `isPrefixOf` b
                            || ("./" ++ a ++ "/") `isPrefixOf` b
                            || "./" ++ a == b
          onlysubdirs suffixes = filter $ or . mapM isParentDir suffixes

slurpRevision :: RepoPatch p => [DarcsFlag] -> Repository p C(r u r) -> IO (Tree IO)
slurpRevision opts r = withDelayedDir "revisioned.showfiles" $ \_ -> do
  getNonrangeMatch r opts
  expand =<< readPlainTree "."


\end{code}
