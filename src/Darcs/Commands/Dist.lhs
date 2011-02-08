%  Copyright (C) 2003 David Roundy
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

\darcsCommand{dist}
\begin{code}
module Darcs.Commands.Dist ( dist ) where
import System.Directory ( setCurrentDirectory )
import Workaround ( getCurrentDirectory )
import System.Exit ( ExitCode(..), exitWith )
import System.Cmd ( system )
import System.FilePath.Posix ( takeFileName, (</>) )
import Data.Char ( isAlphaNum )
import Control.Monad ( when )
import Codec.Archive.Tar ( pack, write )
import Codec.Archive.Tar.Entry ( entryPath )
import Codec.Compression.GZip ( compress )
import Prelude hiding ( writeFile )
import Data.ByteString.Lazy ( writeFile )

import Darcs.Commands ( DarcsCommand(..),
                        nodefaults )
import Darcs.Arguments ( DarcsFlag(Verbose, DistName, SetScriptsExecutable), distnameOption,
                         workingRepoDir, matchOne, storeInMemory,
                         setScriptsExecutableOption )
import Darcs.Match ( getNonrangeMatch, haveNonrangeMatch )
import Darcs.Repository ( amInRepository, withRepoReadLock, RepoJob(..), --withRecorded,
                          setScriptsExecutable,
                          createPartialsPristineDirectoryTree )
import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Lock ( withTempDir )
import Darcs.RepoPath ( AbsolutePath, toFilePath )
import Darcs.Utils ( withCurrentDirectory )

distDescription :: String
distDescription = "Create a distribution tarball."

distHelp :: String
distHelp =
 "The `darcs dist' command creates a compressed archive (a `tarball') in\n" ++
 "the repository's root directory, containing the recorded state of the\n" ++
 "working tree (unrecorded changes and the _darcs directory are\n" ++
 "excluded).\n" ++
 "\n" ++
 "If a predist command is set (see `darcs setpref'), that command will\n" ++
 "be run on the tarball contents prior to archiving.  For example,\n" ++
 "autotools projects would set it to `autoconf && automake'.\n" ++
 "\n" ++
 "By default, the tarball (and the top-level directory within the\n" ++
 "tarball) has the same name as the repository, but this can be\n" ++
 "overridden with the --dist-name option.\n"

 -- FIXME: this is tedious and ugly.
 {-
 ++ "\n" ++
 "Suppose you use a version numbering scheme `major.minor.patch', and\n" ++
 "you tag each release `major.minor'.  You can then calculate the\n" ++
 "version number by taking the newest tag and appending a dot and the\n" ++
 "number of patches since that tag.  If you use the directory name as\n" ++
 "the project name, you can make tarballs of the form name-version.tgz\n" ++
 "using the following shell script:\n" ++
 "\n" ++
 "  major_minor=$(darcs show tags | head -1) &&\n" ++
 "  patch_level=$(($(darcs changes --count --from-tag .) - 1)) &&\n" ++
 "  version=$major_minor.$patch_level &&\n" ++
 "  project=${PWD##*/} &&\n" ++
 "  darcs dist --dist-name \"$project\"-\"$version\".tar.gz\n"
 -}

dist :: DarcsCommand
dist = DarcsCommand {commandProgramName = "darcs",
                     commandName = "dist",
                     commandHelp = distHelp,
                     commandDescription = distDescription,
                     commandExtraArgs = 0,
                     commandExtraArgHelp = [],
                     commandCommand = distCmd,
                     commandPrereq = amInRepository,
                     commandGetArgPossibilities = return [],
                     commandArgdefaults = nodefaults,
                     commandAdvancedOptions = [],
                     commandBasicOptions = [distnameOption,
                                              workingRepoDir,
                                              matchOne,
                                              setScriptsExecutableOption,
                                              storeInMemory]}

distCmd :: [DarcsFlag] -> [String] -> IO ()
distCmd opts _ = withRepoReadLock opts $ RepoJob $ \repository -> do
  distname <- getDistName opts
  verb <- return $ Verbose `elem` opts
  predist <- getPrefval "predist"
  formerdir <- getCurrentDirectory
  resultfile <- return (formerdir</>distname++".tar.gz")
  withTempDir "darcsdist" $ \tempdir -> do
    setCurrentDirectory (formerdir)
    withTempDir (toFilePath tempdir </> takeFileName distname) $ \ddir -> do
      if haveNonrangeMatch opts
        then withCurrentDirectory ddir $ getNonrangeMatch repository opts
        else createPartialsPristineDirectoryTree repository [""] (toFilePath ddir)
      ec <- case predist of Nothing -> return ExitSuccess
                            Just pd -> system pd
      if (ec == ExitSuccess)
          then
              do
              withCurrentDirectory ddir $
                  when (SetScriptsExecutable `elem` opts) setScriptsExecutable
              doDist verb tempdir ddir resultfile
          else
              do
              putStrLn "Dist aborted due to predist failure"
              exitWith ec

-- | This function performs the actual distribution action itself.
-- NB - it does /not/ perform the pre-dist, that should already
-- have completed successfully before this is invoked.
doDist :: Bool -> AbsolutePath -> AbsolutePath -> FilePath -> IO ()
doDist verb tempdir ddir resultfile = do
  setCurrentDirectory (toFilePath tempdir)
  let safeddir = safename $ takeFileName $ toFilePath ddir
  entries <- pack "." [safeddir]
  when verb $ putStr $ unlines $ map entryPath entries
  writeFile resultfile $ compress $ write entries
  putStrLn $ "Created dist as "++resultfile
  where
    safename n@(c:_) | isAlphaNum c  = n
    safename n = "./" ++ n

guessRepoName :: IO String
guessRepoName = do
  pwd <- getCurrentDirectory
  if '/' `elem` pwd
     then return $ reverse $ takeWhile (/='/') $ reverse pwd
     else return "cantguessreponame"

getDistName :: [DarcsFlag] -> IO String
getDistName (DistName dn:_) = return dn
getDistName (_:fs) = getDistName fs
getDistName _ = guessRepoName
\end{code}

