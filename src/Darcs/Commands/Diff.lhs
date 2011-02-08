%  Copyright (C) 2003-2004 David Roundy
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

\darcsCommand{diff}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Diff ( diffCommand ) where

import System.FilePath.Posix ( takeFileName )
import Workaround ( getCurrentDirectory )
import Darcs.Utils ( askUser, withCurrentDirectory )
import Control.Monad ( when )
import Data.List ( (\\) )
import Data.Maybe ( fromMaybe )

import Storage.Hashed.Plain( writePlainTree )

import Darcs.External( diffProgram )
import CommandLine ( parseCmd )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag(DiffFlags, DiffCmd,
                                   LastN, AfterPatch),
                        matchRange, storeInMemory,
                        diffCmdFlag, diffflags, unidiff,
                         workingRepoDir, fixSubPaths,
                      )
import Darcs.Flags ( isUnified )
import Darcs.Patch.PatchInfoAnd ( info )
import Darcs.RepoPath ( AbsolutePath, SubPath, toFilePath, sp2fn )
import Darcs.Match ( getPartialFirstMatch, getPartialSecondMatch,
                     firstMatch, secondMatch,
                     matchFirstPatchset, matchSecondPatchset )
import Darcs.Repository ( withRepository, RepoJob(..), readRepo,
                          amInRepository,
                          createPristineDirectoryTree,
                          createPartialsPristineDirectoryTree )
import Darcs.Patch.Set ( PatchSet, newset2RL )
import Darcs.Repository.State ( readUnrecorded )
import Darcs.Patch ( RepoPatch )
import Darcs.Witnesses.Ordered ( mapRL )
import Darcs.Patch.Info ( PatchInfo, humanFriendly )
import Darcs.External ( execPipeIgnoreError )
import Darcs.Lock ( withTempDir )
import Darcs.Witnesses.Sealed ( unseal )
import Printer ( Doc, putDoc, vcat, empty, ($$) )
#include "impossible.h"

#include "gadts.h"

diffDescription :: String
diffDescription = "Create a diff between two versions of the repository."

diffHelp :: String
diffHelp =
 "Diff can be used to create a diff between two versions which are in your\n"++
 "repository.  Specifying just --from-patch will get you a diff against\n"++
 "your working copy.  If you give diff no version arguments, it gives\n"++
 "you the same information as whatsnew except that the patch is\n"++
 "formatted as the output of a diff command\n"

diffCommand :: DarcsCommand
diffCommand = DarcsCommand {commandProgramName = "darcs",
                             commandName = "diff",
                             commandHelp = diffHelp,
                             commandDescription = diffDescription,
                             commandExtraArgs = -1,
                             commandExtraArgHelp
                                 = ["[FILE or DIRECTORY]..."],
                             commandCommand = diffCmd,
                             commandPrereq = amInRepository,
                             commandGetArgPossibilities = return [],
                             commandArgdefaults = nodefaults,
                             commandAdvancedOptions = [],
                             commandBasicOptions = [matchRange,
                                                     diffCmdFlag,
                                                     diffflags, unidiff,
                                                     workingRepoDir, storeInMemory]}
\end{code}

\begin{options}
--diff-opts
\end{options}

Diff calls an external ``diff'' command to do the actual work, and passes
any unrecognized flags to this diff command.  Thus you can call
\begin{verbatim}
% darcs diff -t 0.9.8 -t 0.9.10 -- -u
\end{verbatim}
to get a diff in the unified format.  Actually, thanks to the wonders of
getopt you need the ``\verb!--!'' shown above before any arguments to diff.
You can also specify additional arguments to diff using the
\verb!--diff-opts! flag.  The above command would look like this:
\begin{verbatim}
% darcs diff --diff-opts -u -t 0.9.8 -t 0.9.10
\end{verbatim}
This may not seem like an improvement, but it really pays off when you want
to always give diff the same options.  You can do this by adding
\begin{verbatim}
% diff diff-opts -udp
\end{verbatim}
to your \verb!_darcs/prefs/defaults! file.

\begin{code}
getDiffOpts :: [DarcsFlag] -> [String]
getDiffOpts opts | isUnified opts = "-u" : get_nonU_diff_opts opts
                   | otherwise      = get_nonU_diff_opts opts
    where get_nonU_diff_opts (DiffFlags f:fs) = f : get_nonU_diff_opts fs
          get_nonU_diff_opts (_:fs) = get_nonU_diff_opts fs
          get_nonU_diff_opts [] = []

hasDiffCmdFlag :: [DarcsFlag] -> Bool
hasDiffCmdFlag (DiffCmd _:_) = True
hasDiffCmdFlag (_:t) = hasDiffCmdFlag t
hasDiffCmdFlag []  = False

-- | Returns the command we should use for diff as a tuple (command, arguments).
-- This will either be whatever the user specified via --diff-command  or the
-- default 'diffProgram'.  Note that this potentially involves parsing the
-- user's diff-command, hence the possibility for failure with an exception.
getDiffCmdAndArgs :: String -> [DarcsFlag] -> String -> String
                      -> Either String (String, [String])
getDiffCmdAndArgs cmd opts f1 f2 = helper opts where
  helper (DiffCmd c:_) =
    case parseCmd [ ('1', f1) , ('2', f2) ] c of
    Left err        -> Left $ show err
    Right ([],_)    -> bug $ "parseCmd should never return empty list"
    Right ((h:t),_) -> Right (h,t)
  helper [] = -- if no command specified, use 'diff'
    Right (cmd, ("-rN":getDiffOpts opts++[f1,f2]))
  helper (_:t) = helper t
\end{code}

If you want to view only the differences to one or more files, you can do
so with a command such as
\begin{verbatim}
% darcs diff foo.c bar.c baz/
\end{verbatim}

\begin{options}
--diff-command
\end{options}

You can use a different program to view differences by including
the flag \verb!--diff-command!, e.g.
\begin{verbatim}
--diff-command 'opendiff %1 %2'.
\end{verbatim}
The \verb!%1! and \verb!%2!  are replaced with the two versions to be
merged.  The above example works with the FileMerge.app tool that comes with
Apple's developer tools.  To use xxdiff, you would use
\begin{verbatim}
--diff-command 'xxdiff %1 %2'
\end{verbatim}
To use \verb!kdiff3!, you can use
\begin{verbatim}
--diff-command 'kdiff3 %1 %2'
\end{verbatim}

Note that the command is split into space-separated words and the first one is
\verb!exec!ed with the rest as arguments---it is not a shell command.  Also
the substitution of the \verb!%! escapes is only done on complete words.
See \ref{resolution} for how you might work around this fact, for example,
with Emacs' Ediff package.

Note also that the \verb!--diff-opts! flag is ignored if you use this option.

\begin{code}
diffCmd :: [DarcsFlag] -> [String] -> IO ()
diffCmd opts args
  | not (null [i | LastN i <- opts]) &&
      not (null [p | AfterPatch p <- opts]) =
        fail $ "using --patch and --last at the same time with the 'diff'" ++
          " command doesn't make sense. Use --from-patch to create a diff" ++
          " from this patch to the present, or use just '--patch' to view" ++
          " this specific patch."
  | null args = doDiff opts Nothing
  | otherwise = doDiff opts . Just =<< fixSubPaths opts args

doDiff :: [DarcsFlag] -> Maybe [SubPath] ->  IO ()
doDiff opts sps = withRepository opts $ RepoJob $ \repository -> do
  let pathList = map sp2fn `fmap` sps
  formerdir <- getCurrentDirectory
  withTempDirs (takeFileName formerdir) $ \odir ndir -> do
    if firstMatch opts
      then withCurrentDirectory odir . getPartialFirstMatch repository opts $
        fromMaybe [] pathList
      else case pathList of
        Nothing -> createPristineDirectoryTree repository $ toFilePath odir
        Just pl -> createPartialsPristineDirectoryTree repository pl $ toFilePath odir
    if secondMatch opts
       then withCurrentDirectory ndir . getPartialSecondMatch repository opts $
               fromMaybe [] pathList
       else withCurrentDirectory formerdir $
               readUnrecorded repository sps >>= (flip writePlainTree (toFilePath ndir))
    thediff <- withCurrentDirectory (toFilePath odir ++ "/..") $
                   case pathList of
                   Nothing -> rundiff (takeFileName $ toFilePath odir) (takeFileName $ toFilePath ndir)
                   Just fs -> vcat `fmap`
                         mapM (\f -> rundiff
                               (takeFileName (toFilePath odir) ++ "/" ++ toFilePath f)
                               (takeFileName (toFilePath ndir) ++ "/" ++ toFilePath f)) fs
    morepatches <- readRepo repository
    putDoc $ changelog (getDiffInfo opts morepatches)
            $$ thediff
    where rundiff :: String -> String -> IO Doc
          rundiff f1 f2 = do
            cmd <- diffProgram
            case getDiffCmdAndArgs cmd opts f1 f2 of
             Left err -> fail err
             Right (d_cmd, d_args) ->
              let other_diff = hasDiffCmdFlag opts in
              do when other_diff $ putStrLn $
                   "Running command '" ++ unwords (d_cmd:d_args) ++ "'"
                 output <- execPipeIgnoreError d_cmd d_args empty
                 when other_diff $ do
-- Warning:  A do-notation statement discarded a result of type String.
                    _ <- askUser "Hit return to move on..."
                    return ()
                 return output

          withTempDirs :: String -> (AbsolutePath -> AbsolutePath -> IO a) -> IO a
          withTempDirs x f = withTempDir ("old-" ++ x) $ \odir ->
            withTempDir ("new-" ++ x) $ \ndir -> f odir ndir

getDiffInfo :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x) -> [PatchInfo]
getDiffInfo opts ps =
    let infos = mapRL info . newset2RL
        handle (match_cond, do_match)
          | match_cond opts = unseal infos (do_match opts ps)
          | otherwise = infos ps
    in handle (secondMatch, matchSecondPatchset)
         \\ handle (firstMatch, matchFirstPatchset)

changelog :: [PatchInfo] -> Doc
changelog pis = vcat $ map humanFriendly pis
\end{code}

