%  Copyright (C) 2002-2003 David Roundy
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

\darcsCommand{record}
\begin{code}
{-# LANGUAGE CPP, PatternGuards #-}

module Darcs.Commands.Record ( record, commit, getDate, getLog,
                               askAboutDepends
                             ) where
import qualified Ratified( hGetContents )
import Control.Applicative ( (<$>) )
import Control.Exception.Extensible ( handleJust )
import Control.Monad ( when )
import System.IO ( stdin )
import Data.List ( sort, isPrefixOf, union )
import Data.Char ( ord )
import System.Exit ( exitWith, exitFailure, ExitCode(..) )
import System.Directory ( removeFile )
import Data.Maybe ( isJust, catMaybes, fromMaybe )
import qualified Data.ByteString as B ( hPut )

import Darcs.Lock ( readLocaleFile, writeLocaleFile, worldReadableTemp, appendToFile )
import Darcs.Patch.PatchInfoAnd ( info, n2pia, PatchInfoAnd )
import Darcs.Repository ( Repository, amInRepository, withRepoLock, RepoJob(..),
                          withGutsOf,
                    readTentativeRepo,
                    tentativelyAddPatch, finalizeRepositoryChanges
                        , testTentative
                        , invalidateIndex, unrecordedChanges )
import Darcs.Patch ( RepoPatch, Patchy, PrimOf, PrimPatch, namepatch, summaryFL, anonymous,
                     adddeps, fromPrims )
import Darcs.Patch.Set ( PatchSet(..) )
import Darcs.Witnesses.Eq ( unsafeCompare )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (+>+),
                             reverseRL, mapFL, mapFL_FL, nullFL )
import Darcs.Witnesses.Sealed
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Patch.Choices ( patchChoicesTps, tpPatch,
                             forceFirsts, getChoices, tag )
import Darcs.SelectChanges ( selectChanges, WhichChanges(..),
                             selectionContext, selectionContextPrim,
                             runSelection
                           )
import Darcs.RepoPath ( FilePathLike, SubPath, toFilePath )
import Darcs.Commands ( DarcsCommand(..), nodefaults, commandStub )
import Darcs.Commands.WhatsNew ( announceFiles, filterExistingFiles )
import Darcs.Arguments ( DarcsFlag( PromptLongComment, NoEditLongComment,
                                    EditLongComment, LogFile, Pipe,
                                    PatchName, AskDeps, All ),
                         fileHelpAuthor,
                         getAuthor, workingRepoDir, lookforadds,
                         fixSubPaths, setEnvDarcsFiles,
                         askLongComment, askdeps, patchSelectFlag,
                         allPipeInteractive, leaveTestDir, test,
                         author, patchnameOption, umaskOption, ignoretimes,
                         nocompress, rmlogfile, logfile, listRegisteredFiles,
                         setScriptsExecutableOption )
import Darcs.Flags (willRemoveLogFile, diffingOpts, compression, isInteractive)
import Darcs.Utils ( askUser, promptYorn, PromptConfig(..), promptChar, editFile, clarifyErrors )
import Progress ( debugMessage)
import Darcs.ProgressPatches( progressFL)
import IsoDate ( getIsoDateTime, cleanLocalDate )
import Printer ( hPutDocLn, text, wrapText, ($$) )
import ByteStringUtils ( encodeLocale )
#include "impossible.h"
#include "gadts.h"

recordDescription :: String
recordDescription = "Create a patch from unrecorded changes."

recordHelp :: String
recordHelp =
 "The `darcs record' command is used to create a patch from changes in\n" ++
 "the working tree.  If you specify a set of files and directories,\n" ++
 "changes to other files will be skipped.\n" ++
 "\n" ++ recordHelp' ++
 "\n" ++ recordHelp''

record :: DarcsCommand
record = DarcsCommand {commandProgramName = "darcs",
                       commandName = "record",
                       commandHelp = recordHelp,
                       commandDescription = recordDescription,
                       commandExtraArgs = -1,
                       commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                       commandCommand = recordCmd,
                       commandPrereq = amInRepository,
                       commandGetArgPossibilities = listRegisteredFiles,
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [logfile, rmlogfile,
                                                   nocompress, ignoretimes,
                                                   umaskOption,
                                                   setScriptsExecutableOption],
                       commandBasicOptions = [patchnameOption, author,
                                               test,
                                               leaveTestDir,
                                               allPipeInteractive,
                                               askdeps,
                                               askLongComment,
                                               lookforadds,
                                               workingRepoDir]}

commitDescription :: String
commitDescription = "Redirect the user to record, push or send."

commitHelp :: String
commitHelp =
 "This command does not do anything.\n"++
 "If you want to save changes locally, use the `darcs record' command.\n"++
 "If you want to save a recorded patch to another repository, use the\n"++
 "`darcs push' or `darcs send' commands instead.\n"

commit :: DarcsCommand
commit = commandStub "commit" commitHelp commitDescription record

recordCmd :: [DarcsFlag] -> [String] -> IO ()
recordCmd opts args = do
    checkNameIsNotOption opts
    withRepoLock opts $ RepoJob $ \repository -> do
    files <- if null args then return Nothing
        else Just . sort <$> fixSubPaths opts args
    when (files == Just []) $ fail "No valid arguments were given."
    announceFiles files "Recording changes in"
    existing_files <- maybe (return Nothing)
        (fmap Just . filterExistingFiles repository) files
    when (existing_files == Just []) $
       fail "None of the files you specified exist!"
    debugMessage "About to get the unrecorded changes."
    changes <- unrecordedChanges (diffingOpts opts) repository files
    debugMessage "I've gotten unrecorded."
    case allow_empty_with_askdeps changes of
-- Warning:  A do-notation statement discarded a result of type String.
      Nothing -> do when (Pipe `elem` opts) $ do _ <- getDate opts
                                                 return ()
                    putStrLn "No changes!"
      Just ch -> doRecord repository opts existing_files ch
    where allow_empty_with_askdeps :: FL p C(x y) -> Maybe (FL p C(x y))
          allow_empty_with_askdeps NilFL
              | AskDeps `elem` opts = Just NilFL
              | otherwise = Nothing
          allow_empty_with_askdeps p = Just p

 -- check that what we treat as the patch name is not accidentally a command
 -- line flag
checkNameIsNotOption :: [DarcsFlag] -> IO ()
checkNameIsNotOption opts = do
    let patchNames = [n | PatchName n <- opts]
    when (length patchNames == 1) $ do
        let n = head patchNames
            oneLetterName = length n == 1 || (length n == 2 && head n == '-')
        if (oneLetterName && not (elem All opts))
            then do
                let keepAsking = do
                    yorn <- promptYorn ("You specified " ++ show n ++ " as the patch name. Is that really what you want?")
                    case yorn of
                        'y' -> return ()
                        'n' -> do
                                   putStrLn "Okay, aborting the record."
                                   exitFailure
                        _   -> keepAsking
                keepAsking
            else return ()


doRecord :: RepoPatch p => Repository p C(r u r) -> [DarcsFlag] -> Maybe [SubPath] -> FL (PrimOf p) C(r x) -> IO ()
doRecord repository opts files ps = do
    let make_log = worldReadableTemp "darcs-record"
    date <- getDate opts
    my_author <- getAuthor opts
    debugMessage "I'm slurping the repository."
    debugMessage "About to select changes..."
    (chs :> _ ) <- runSelection (selectChanges First ps) $
                  selectionContextPrim "record" opts (Just primSplitter)
                                       (map toFilePath $ fromMaybe [] files)
    when (is_empty_but_not_askdeps chs) $
              do putStrLn "Ok, if you don't want to record anything, that's fine!"
                 exitWith ExitSuccess
    handleJust onlySuccessfulExits (\_ -> return ()) $
             do deps <- if AskDeps `elem` opts
                        then askAboutDepends repository chs opts []
                        else return []
                when (AskDeps `elem` opts) $ debugMessage "I've asked about dependencies."
                if nullFL chs && null deps
                  then putStrLn "Ok, if you don't want to record anything, that's fine!"
                  else do setEnvDarcsFiles chs
                          (name, my_log, logf) <- getLog opts Nothing make_log chs
                          debugMessage ("Patch name as received from getLog: " ++ show (map ord name))
                          doActualRecord repository opts name date
                                 my_author my_log logf deps chs
    where is_empty_but_not_askdeps l
              | AskDeps `elem` opts = False
                                      -- a "partial tag" patch; see below.
              | otherwise = nullFL l

doActualRecord :: RepoPatch p => Repository p C(r u r) -> [DarcsFlag] -> String -> String -> String
                 -> [String] -> Maybe String
                 -> [PatchInfo] -> FL (PrimOf p) C(r x) -> IO ()
doActualRecord repository opts name date my_author my_log logf deps chs =
              do debugMessage "Writing the patch file..."
                 mypatch <- namepatch date name my_author my_log $
                            fromPrims $ progressFL "Writing changes:" chs
-- Warning:  A do-notation statement discarded a result of type Repository p r u x.
                 _ <- tentativelyAddPatch repository (compression opts) $ n2pia $ adddeps mypatch deps
                 invalidateIndex repository
                 debugMessage "Applying to pristine..."
                 rc <- testTentative repository
                 when (rc /= ExitSuccess) $ do
                     when (not $ isInteractive opts) $ exitWith rc `clarifyErrors` failuremessage
                     putStrLn $ "Looks like you have a bad patch: '"++name++"'"
                     let prompt = "Shall I record it anyway?"
                     yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
                     case yn of
                       'y' -> return ()
                       _ -> exitWith rc `clarifyErrors` failuremessage
                 withGutsOf repository (finalizeRepositoryChanges repository)
                                    `clarifyErrors` failuremessage
                 debugMessage "Syncing timestamps..."
                 when (isJust logf) $ removeFile (fromJust logf)
                 putStrLn $ "Finished recording patch '"++name++"'"
    where failuremessage = "Failed to record patch '"++name++"'" ++
                           case logf of Just lf -> "\nLogfile left in "++lf++"."
                                        Nothing -> ""

recordHelp' :: String
recordHelp' =
 "Every patch has a name, an optional description, an author and a date.\n" ++
 "\n" ++
 "The patch name should be a short sentence that concisely describes the\n" ++
 "patch, such as `Add error handling to main event loop.'  You can\n" ++
 "supply it in advance with the -m option, or provide it when prompted.\n" ++
 "\n" ++
 "The patch description is an optional block of free-form text.  It is\n" ++
 "used to supply additional information that doesn't fit in the patch\n" ++
 "name.  For example, it might include a rationale of WHY the change was\n" ++
 "necessary.  By default Darcs asks if you want to add a description;\n" ++
 "the --edit-long-comment and --skip-long-comment can be used to answer\n" ++
 "`yes' or `no' (respectively) to this prompt.  Finally, the --logfile\n" ++
 "option allows you to supply a file that already contains the patch\n" ++
 "name (first line) and patch description (subsequent lines).  This is\n" ++
 "useful if a previous record failed and left a darcs-record-0 file.\n" ++
 "\n" ++
 unlines fileHelpAuthor ++
 "\n" ++
 "The patch date is generated automatically.  It can only be spoofed by\n" ++
 "using the --pipe option.\n"

getDate :: [DarcsFlag] -> IO String
getDate opts
 | Pipe `elem` opts = do cleanLocalDate `fmap` askUser "What is the date? "
getDate _ = getIsoDateTime

data PName = FlagPatchName String | PriorPatchName String | NoPatchName

getLog :: forall prim C(x y) . (Patchy prim, PrimPatch prim) => [DarcsFlag] -> Maybe (String, [String]) -> IO String -> FL prim C(x y) ->
           IO (String, [String], Maybe String)
getLog opts m_old make_log chs = gl opts
    where patchname_specified = patchname_helper opts
          patchname_helper (PatchName n:_) | "TAG " `isPrefixOf` n = FlagPatchName $ '.':n
                                           | otherwise             = FlagPatchName n
          patchname_helper (_:fs) = patchname_helper fs
          patchname_helper [] = case m_old of Just (p,_) -> PriorPatchName p
                                              Nothing    -> NoPatchName
          default_log = case m_old of
                          Nothing    -> []
                          Just (_,l) -> l
          gl (Pipe:_) = do p <- case patchname_specified of
                                  FlagPatchName p  -> return p
                                  PriorPatchName p -> return p
                                  NoPatchName      -> prompt_patchname False
                           putStrLn "What is the log?"
                           thelog <- lines `fmap` Ratified.hGetContents stdin
                           return (p, thelog, Nothing)
          gl (LogFile f:fs) =
              do -- round 1 (patchname)
                 mlp <- lines `fmap` readLocaleFile f `catch` (\_ -> return [])
                 firstname <- case (patchname_specified, mlp) of
                                (FlagPatchName  p, []) -> return p
                                (_, p:_)               -> return p -- logfile trumps prior!
                                (PriorPatchName p, []) -> return p
                                (NoPatchName, [])      -> prompt_patchname True
                 -- round 2
                 append_info f firstname
-- Warning:  A do-notation statement discarded a result of type ExitCode.
                 when (EditLongComment `elem` fs) $ do _ <- editFile f
                                                       return ()
                 (name, thelog, _) <- read_long_comment f firstname
                 let toRemove = if willRemoveLogFile opts
                        then Just $ toFilePath f
                        else Nothing
                 return (name, thelog, toRemove)
          gl (EditLongComment:_) =
                  case patchname_specified of
                    FlagPatchName  p -> actually_get_log p
                    PriorPatchName p -> actually_get_log p
                    NoPatchName      -> prompt_patchname True >>= actually_get_log
          gl (NoEditLongComment:_) =
                  case patchname_specified of
                    FlagPatchName  p
                        | Just ("",_) <- m_old ->
                                       return (p, default_log, Nothing) -- rollback -m
                    FlagPatchName  p -> return (p, default_log, Nothing) -- record (or amend) -m
                    PriorPatchName p -> return (p, default_log, Nothing) -- amend
                    NoPatchName      -> do p <- prompt_patchname True -- record
                                           return (p, [], Nothing)
          gl (PromptLongComment:fs) =
                  case patchname_specified of
                    FlagPatchName p -> prompt_long_comment p -- record (or amend) -m
                    _               -> gl fs
          gl (_:fs) = gl fs
          gl [] = case patchname_specified of
                    FlagPatchName  p -> return (p, default_log, Nothing)  -- record (or amend) -m
                    PriorPatchName "" -> prompt_patchname True >>= prompt_long_comment
                    PriorPatchName p -> return (p, default_log, Nothing)
                    NoPatchName -> prompt_patchname True >>= prompt_long_comment
          prompt_patchname retry =
            do n <- askUser "What is the patch name? "
               if n == "" || "TAG " `isPrefixOf` n
                  then if retry then prompt_patchname retry
                                else fail "Bad patch name!"
                  else return n
          prompt_long_comment oldname =
            do yorn <- promptYorn "Do you want to add a long comment?"
               if yorn == 'y' then actually_get_log oldname
                              else return (oldname, [], Nothing)
          actually_get_log p = do logf <- make_log
                                  -- TODO: make sure encoding used for logf is the same everywhere
                                  -- probably should be locale because the editor will assume it
                                  writeLocaleFile logf $ unlines $ p : default_log
                                  append_info logf p
-- Warning:  A do-notation statement discarded a result of type ExitCode.
                                  _ <- editFile logf
                                  read_long_comment logf p
          read_long_comment :: FilePathLike p => p -> String -> IO (String, [String], Maybe p)
          read_long_comment f oldname =
              do t <- (lines.filter (/='\r')) `fmap` readLocaleFile f
                 case t of [] -> return (oldname, [], Just f)
                           (n:ls) -> return (n, takeWhile
                                             (not.(eod `isPrefixOf`)) ls,
                                             Just f)
          append_info f oldname =
              do fc <- readLocaleFile f
                 appendToFile f $ \h ->
                     do case fc of
                          _ | null (lines fc) -> B.hPut h (encodeLocale (oldname ++ "\n"))
                            | last fc /= '\n' -> B.hPut h (encodeLocale "\n")
                            | otherwise       -> return ()
                        hPutDocLn h $ text eod
                            $$ text ""
                            $$ wrapText 75
                               ("Place the long patch description above the "++
                                eod++
                                " marker.  The first line of this file "++
                                "will be the patch name.")
                            $$ text ""
                            $$ text "This patch contains the following changes:"
                            $$ text ""
                            $$ summaryFL chs

eod :: String
eod = "***END OF DESCRIPTION***"
\end{code}

\begin{options}
--ask-deps
\end{options}

Each patch may depend on any number of previous patches.  If you choose to
make your patch depend on a previous patch, that patch is required to be
applied before your patch can be applied to a repository.  This can be used, for
example, if a piece of code requires a function to be defined, which was
defined in an earlier patch.

If you want to manually define any dependencies for your patch, you can use
the \verb!--ask-deps! flag, and darcs will ask you for the patch's
dependencies.

It is possible to record a patch which has no actual changes but which
has specific dependencies.  This type of patch can be thought of as a
``partial tag''.  The \verb!darcs tag! command will record a patch
with no actual changes but which depends on the entire current
inventory of the repository.  The \verb!darcs record --ask-deps! with
no selected changes will record a patch that depends on only those
patches selected via the \verb!--ask-deps! operation, resulting in a
patch which describes a set of patches; the presence of this primary
patch in a repository implies the presence of (at least) the
depended-upon patches.

\begin{code}
askAboutDepends :: forall p C(r u t y) . RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(t y) -> [DarcsFlag] -> [PatchInfo] -> IO [PatchInfo]
askAboutDepends repository pa' opts olddeps = do
  -- ideally we'd just default the olddeps to yes but still ask about them.
  -- SelectChanges doesn't currently (17/12/09) offer a way to do this so would
  -- have to have this support added first.
  pps <- readTentativeRepo repository
  pa <- n2pia `fmap` anonymous (fromPrims pa')
  FlippedSeal ps <- return
                      ((case pps of
                          PatchSet x _ -> FlippedSeal ((reverseRL x)+>+(pa:>:NilFL)))
                         :: FlippedSeal (FL (PatchInfoAnd p)) C(y))
  let (pc, tps) = patchChoicesTps ps
      tas = case catMaybes (mapFL (\tp -> if pa `unsafeCompare` (tpPatch tp) || info (tpPatch tp) `elem` olddeps
                                          then Just (tag tp) else Nothing) tps) of

                [] -> error "askAboutDepends: []"
                tgs -> tgs
  Sealed2 ps' <- return $ case getChoices (forceFirsts tas pc) of _ :> mc :> _ -> Sealed2 $ mapFL_FL tpPatch mc
  (deps:>_) <- runSelection (selectChanges FirstReversed ps') $
                                        selectionContext "depend on" (filter askdep_allowed opts) Nothing []
  return $ olddeps `union` mapFL info deps
 where
       askdep_allowed = not . patchSelectFlag


onlySuccessfulExits :: ExitCode -> Maybe ()
onlySuccessfulExits ExitSuccess = Just ()
onlySuccessfulExits _ = Nothing

recordHelp'' :: String
recordHelp'' =
 "If a test command has been defined with `darcs setpref', attempting to\n" ++
 "record a patch will cause the test command to be run in a clean copy\n" ++
 "of the working tree (that is, including only recorded changes).  If\n" ++
 "the test fails, you will be offered to abort the record operation.\n" ++
 "\n" ++
 "The --set-scripts-executable option causes scripts to be made\n" ++
 "executable in the clean copy of the working tree, prior to running the\n" ++
 "test.  See `darcs get' for an explanation of the script heuristic.\n" ++
 "\n" ++
 "If your test command is tediously slow (e.g. `make all') and you are\n" ++
 "recording several patches in a row, you may wish to use --no-test to\n" ++
 "skip all but the final test.\n"

\end{code}
\begin{options}
--pipe
\end{options}

If you run record with the \verb!--pipe! option, you will be prompted for
the patch date, author, and the long comment. The long comment will extend
until the end of file or stdin is reached (ctrl-D on Unixy systems, ctrl-Z
on systems running a Microsoft OS).

This interface is intended for scripting darcs, in particular for writing
repository conversion scripts.  The prompts are intended mostly as a useful
guide (since scripts won't need them), to help you understand the format in
which to provide the input. Here's an example of what the \verb!--pipe!
prompts look like:

\begin{verbatim}
 What is the date? Mon Nov 15 13:38:01 EST 2004
 Who is the author? David Roundy
 What is the log? One or more comment lines
\end{verbatim}


\begin{options}
--interactive
\end{options}

By default, \verb!record! works interactively. Probably the only thing you need
to know about using this is that you can press \verb!?! at the prompt to be
shown a list of the rest of the options and what they do. The rest should be
clear from there. Here's a
``screenshot'' to demonstrate:

\begin{verbatim}
hunk ./hello.pl +2
+#!/usr/bin/perl
+print "Hello World!\n";
Shall I record this patch? (2/2) [ynWsfqadjk], or ? for help: ?
How to use record...
y: record this patch
n: don't record it
w: wait and decide later, defaulting to no

s: don't record the rest of the changes to this file
f: record the rest of the changes to this file

d: record selected patches
a: record all the remaining patches
q: cancel record

j: skip to next patch
k: back up to previous patch
h or ?: show this help

<Space>: accept the current default (which is capitalized)

\end{verbatim}
What you can't see in that ``screenshot'' is that \verb!darcs! will also try to use
color in your terminal to make the output even easier to read.
