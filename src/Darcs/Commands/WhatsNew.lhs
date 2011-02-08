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

\darcsCommand{whatsnew}
\begin{code}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Commands.WhatsNew ( whatsnew, status, announceFiles,
    filterExistingFiles ) where
import System.Exit ( ExitCode(..), exitWith )
import Data.List ( sort, (\\) )
import Data.Maybe ( fromMaybe )
import Control.Monad ( when )
import Control.Applicative ( (<$>) )

import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias )
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir, lookforadds,
                        ignoretimes, noskipBoring,
                         unified, summary,
                         fixSubPaths,
                        listRegisteredFiles,
                      )
import Darcs.Flags( isUnified, diffingOpts )

import Darcs.Repository ( Repository, withRepository, RepoJob(..)
                        , amInRepository, extractOptions
                        , unrecordedChanges, readRecordedAndPending, readRecorded, readUnrecorded )
import Darcs.Repository.State( restrictBoring, applyTreeFilter )
import Darcs.Repository.Prefs ( filetypeFunction )
import Darcs.Patch ( RepoPatch, PrimPatch, PrimOf, plainSummaryPrims, primIsHunk, applyToTree )
import Darcs.Patch.TouchesFiles( choosePreTouching )
import Darcs.Patch.Permutations ( partitionRL )
import Darcs.RepoPath( SubPath, toFilePath )
import Darcs.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Witnesses.Ordered ( FL(..), reverseRL, reverseFL, (:>)(..), nullFL )
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Diff( treeDiff )

import Storage.Hashed.Monad( virtualTreeIO, exists )
import Storage.Hashed( readPlainTree )
import Storage.Hashed( floatPath )

import Printer ( putDocLn, renderString, vcat, text )

whatsnewDescription :: String
whatsnewDescription = "List unrecorded changes in the working tree."

whatsnewHelp :: String
whatsnewHelp =
 "The `darcs whatsnew' command lists unrecorded changes to the working\n" ++
 "tree.  If you specify a set of files and directories, only unrecorded\n" ++
 "changes to those files and directories are listed.\n" ++
 "\n" ++
 "With the --summary option, the changes are condensed to one line per\n" ++
 "file, with mnemonics to indicate the nature and extent of the change.\n" ++
 "The --look-for-adds option causes candidates for `darcs add' to be\n" ++
 "included in the summary output.  Summary mnemonics are as follows:\n" ++
 "\n" ++
 "  `A f' and `A d/' respectively mean an added file or directory.\n" ++
 "  `R f' and `R d/' respectively mean a removed file or directory.\n" ++
 "  `M f -N +M rP' means a modified file, with N lines deleted, M\n" ++
 "  lines added, and P lexical replacements.\n" ++
 "  `f -> g' means a moved file or directory.\n" ++
 "\n" ++
 "  An exclamation mark (!) as in `R! foo.c', means the hunk is known to\n" ++
 "  conflict with a hunk in another patch.  The phrase `duplicated'\n" ++
 "  means the hunk is known to be identical to a hunk in another patch.\n" ++
 "\n" ++
 "By default, `darcs whatsnew' uses Darcs' internal format for changes.\n" ++
 "To see some context (unchanged lines) around each change, use the\n" ++
 "--unified option.  To view changes in conventional `diff' format, use\n" ++
 "the `darcs diff' command; but note that `darcs whatsnew' is faster.\n" ++
 "\n" ++
 "This command exits unsuccessfully (returns a non-zero exit status) if\n" ++
 "there are no unrecorded changes.\n"

whatsnew :: DarcsCommand
whatsnew = DarcsCommand {commandProgramName = "darcs",
                         commandName = "whatsnew",
                         commandHelp = whatsnewHelp,
                         commandDescription = whatsnewDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = whatsnewCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [ignoretimes, noskipBoring],
                         commandBasicOptions = [summary, unified,
                                                 lookforadds,
                                                 workingRepoDir]}

filteredChanges :: RepoPatch p => [DarcsFlag] -> Repository p C(r u t)
                -> Maybe [SubPath] -> IO (Sealed (FL (PrimOf p) C(t)))
filteredChanges opts repo files =
  choosePreTouching (map toFilePath $ fromMaybe [] files) `fmap` unrecordedChanges (diffingOpts opts) repo files

announceFiles :: Maybe [SubPath] -> String -> IO ()
announceFiles Nothing _ = return ()
announceFiles (Just files) message = putStrLn $ message ++ " " ++
    unwords (map show files) ++ ":\n"

filterExistingFiles :: (RepoPatch p) => Repository p C(r u t) -> [SubPath] -> IO [SubPath]
filterExistingFiles repo files = do
      pristine <- readRecordedAndPending repo
      -- TODO this is slightly inefficient, since we should really somehow
      -- extract the unrecorded state as a side-effect of unrecordedChanges
      index <- readUnrecorded repo $ Just files
      nonboring <- restrictBoring index
      working <- applyTreeFilter nonboring `fmap` readPlainTree "."
      let paths = map toFilePath files
          check = virtualTreeIO (mapM exists $ map floatPath paths)
      (in_working, _) <- check working
      (in_pending, _) <- check pristine
      mapM_ maybe_warn $ zip3 paths in_working in_pending
      return [ path | (path, True) <- zip files (zipWith (||) in_working in_pending) ]
    where maybe_warn (file, False, False) =
              putStrLn $ "WARNING: File '"++file++"' does not exist!"
          maybe_warn (file, True, False) | LookForAdds `notElem` extractOptions repo =
              putStrLn $ "WARNING: File '" ++ file ++ "' not in repository!"
          maybe_warn _ = return ()

whatsnewCmd :: [DarcsFlag] -> [String] -> IO ()
whatsnewCmd opts' args
  | LookForAdds `elem` opts' && NoSummary `notElem` opts' =
    -- add Summary to the opts since 'darcs whatsnew --look-for-adds'
    -- implies summary
    withRepository (Summary:opts')
                       $ RepoJob $ \(repository :: Repository p C(r u r)) -> do
    files <- if null args then return Nothing
        else Just <$> fixSubPaths opts' args
    announceFiles files "What's new in"
    Sealed all_changes <- filteredChanges opts' repository files
    Sealed chold <- filteredChanges (opts' \\ [LookForAdds]) repository files
    pristine <- readRecorded repository
    ftf <- filetypeFunction
    cho_adds :> _ <- return $ partitionRL primIsHunk $ reverseFL chold
    cha :> _ <- return $ partitionRL primIsHunk $ reverseFL all_changes

    cho_adds_t <- applyToTree (reverseRL cho_adds) pristine
    cha_t <- applyToTree (reverseRL cha) pristine
    Sealed chn <- unFreeLeft `fmap` treeDiff ftf cho_adds_t cha_t :: IO (Sealed (FL (PrimOf p) C(r)))

    exitOnNoChanges (chn, chold)
    putDocLn $ plainSummaryPrims chold
    printSummary chn
    where lower_as x = vcat $ map (text . l_as) $ lines x
          l_as ('A':x) = 'a':x
          l_as x = x
          exitOnNoChanges :: (FL prim C(x y), FL p C(u v)) -> IO ()
          exitOnNoChanges (NilFL, NilFL) = do putStrLn "No changes!"
                                              exitWith $ ExitFailure 1
          exitOnNoChanges _ = return ()
          printSummary :: PrimPatch prim => FL prim C(x y) -> IO ()
          printSummary NilFL = return ()
          printSummary new = putDocLn $ lower_as $ renderString $ plainSummaryPrims new

whatsnewCmd opts args
  | otherwise =
    withRepository opts $ RepoJob $ \repository -> do
    files <- if null args then return Nothing
        else Just . sort <$> fixSubPaths opts args
    announceFiles files "What's new in"
    Sealed changes <- filteredChanges opts repository files
    when (nullFL changes) $ putStrLn "No changes!" >> (exitWith $ ExitFailure 1)
    printSummary repository changes
       where printSummary :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(r y) -> IO ()
             printSummary _ NilFL = do putStrLn "No changes!"
                                       exitWith $ ExitFailure 1
             printSummary r ch | Summary `elem` opts = putDocLn $ plainSummaryPrims ch
                               | isUnified opts = do pristine <- readRecorded r
                                                     contextualPrintPatch pristine ch
                               | otherwise           = printPatch ch

status :: DarcsCommand
status = (commandAlias "status" Nothing whatsnew)
           { commandCommand = \fs -> commandCommand whatsnew (Summary : LookForAdds : fs)
           , commandDescription = "Alias for `darcs " ++ commandName whatsnew ++ " -ls '."
           }
\end{code}
