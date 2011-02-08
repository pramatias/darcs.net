%  Copyright (C) 2003-2005 David Roundy
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

\darcsCommand{apply}
\begin{code}
{-# LANGUAGE CPP, PatternGuards #-}

module Darcs.Commands.Apply ( apply ) where
import System.Exit ( ExitCode(..), exitWith )
import Prelude hiding ( catch )
import System.IO ( hClose, stdout, stderr )
import Control.Exception.Extensible
                 ( catch, fromException, SomeException, throwIO )
import Control.Monad ( when )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefullyM, info )
import Darcs.SignalHandler ( withSignalsBlocked )
import Darcs.Commands ( DarcsCommand(..), putVerbose )
import Darcs.CommandsAux ( checkPaths )
import Darcs.Arguments ( DarcsFlag( Reply, Interactive, All),
                         matchSeveral,
                         setEnvDarcsPatches,
                         getCc, workingRepoDir,
                        notest, nocompress, applyConflictOptions,
                        useExternalMerge,
                        ignoretimes, getSendmailCmd,
                        reply, verify, listFiles,
                        fixFilePathOrStd, umaskOption,
                        allInteractive, sendmailCmd,
                        leaveTestDir, happyForwarding,
                        dryRun, printDryRunMessageAndExit,
                        setScriptsExecutableOption, restrictPaths,
                        changesReverse, makeScriptsExecutable
                      )
import Darcs.Flags(doHappyForwarding, doReverse, isInteractive)

import qualified Darcs.Arguments as DarcsArguments ( ccApply )
import Darcs.RepoPath ( toFilePath, useAbsoluteOrStd )
import Darcs.Repository ( Repository, SealedPatchSet, withRepoLock, RepoJob(..), amInRepository,
                          tentativelyMergePatches,
                    readRepo,
                    finalizeRepositoryChanges,
                    testTentative,
                    applyToWorking, invalidateIndex
                  )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Patch.Set ( newset2RL )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Patch.Info ( PatchInfo, humanFriendly )
import Darcs.Utils ( PromptConfig(..), promptChar )
import Darcs.Witnesses.Ordered ( FL, RL(..), (:\/:)(..), (:>)(..),
                       mapFL, mapRL, nullFL, reverseFL )
import ByteStringUtils ( linesPS, unlinesPS, gzReadFilePS, gzReadStdin )
import Data.List( (\\) )
import qualified Data.ByteString as B (ByteString, null, init, take, drop)
import qualified Data.ByteString.Char8 as BC (unpack, last, pack)

import Darcs.External ( sendEmail, sendEmailDoc, resendEmail,
                  verifyPS )
import Darcs.Email ( readEmail )
import Darcs.Lock ( withStdoutTemp, readBinFile )
import Darcs.Patch.Depends ( findUncommon, findCommonWithThem )
import Darcs.SelectChanges ( selectChanges, WhichChanges(..),
                             runSelection, selectionContext,
                             filterOutConflicts )
import Darcs.Patch.Bundle ( scanBundle )
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
import Printer ( packedString, vcat, text, empty, renderString )

#include "impossible.h"

#include "gadts.h"

applyDescription :: String
applyDescription = "Apply a patch bundle created by `darcs send'."

applyHelp :: String
applyHelp =
 "The `darcs apply' command takes a patch bundle and attempts to insert\n" ++
 "it into the current repository.  In addition to invoking it directly\n" ++
 "on bundles created by `darcs send', it is used internally by `darcs\n" ++
 "push' and `darcs put' on the remote end of an SSH connection.\n" ++
 "\n" ++
 "If no file is supplied, the bundle is read from standard input.\n" ++
 "\n" ++
 "If given an email instead of a patch bundle, Darcs will look for the\n" ++
 "bundle as a MIME attachment to that email.  Currently this will fail\n" ++
 "if the MIME boundary is rewritten, such as in Courier and Mail.app.\n" ++
 "\n" ++
 "If the `--reply noreply@example.net' option is used, and the bundle is\n" ++
 "attached to an email, Darcs will send a report (indicating success or\n" ++
 "failure) to the sender of the bundle (the To field).  The argument to\n" ++
 "noreply is the address the report will appear to originate FROM.\n" ++
 "\n" ++
 "The --cc option will cause the report to be CC'd to another address,\n" ++
 "for example `--cc reports@lists.example.net,admin@lists.example.net'.\n" ++
 "Using --cc without --reply is undefined.\n" ++
 "\n" ++
 "If gpg(1) is installed, you can use `--verify pubring.gpg' to reject\n" ++
 "bundles that aren't signed by a key in pubring.gpg.\n" ++
 "\n" ++
 "If --test is supplied and a test is defined (see `darcs setpref'), the\n" ++
 "bundle will be rejected if the test fails after applying it.  In that\n" ++
 "case, the rejection email from --reply will include the test output.\n"

stdindefault :: a -> [String] -> IO [String]
stdindefault _ [] = return ["-"]
stdindefault _ x = return x
apply :: DarcsCommand
apply = DarcsCommand {commandProgramName = "darcs",
                      commandName = "apply",
                      commandHelp = applyHelp ++ "\n" ++ applyHelp',
                      commandDescription = applyDescription,
                      commandExtraArgs = 1,
                      commandExtraArgHelp = ["<PATCHFILE>"],
                      commandCommand = applyCmd,
                      commandPrereq = amInRepository,
                      commandGetArgPossibilities = listFiles,
                      commandArgdefaults = const stdindefault,
                      commandAdvancedOptions = [reply, DarcsArguments.ccApply,
                                                  happyForwarding,
                                                  sendmailCmd,
                                                  ignoretimes, nocompress,
                                                  setScriptsExecutableOption, umaskOption,
                                                  restrictPaths, changesReverse],
                      commandBasicOptions = [verify,
                                              allInteractive]++dryRun++
                                              [matchSeveral,
                                               applyConflictOptions,
                                               useExternalMerge,
                                               notest,
                                               leaveTestDir,
                                               workingRepoDir]}

applyCmd :: [DarcsFlag] -> [String] -> IO ()
applyCmd _ [""] = fail "Empty filename argument given to apply!"
applyCmd opts [unfixed_patchesfile] = withRepoLock opts $ RepoJob $ \repository -> do
  patchesfile <- fixFilePathOrStd opts unfixed_patchesfile
  ps <- useAbsoluteOrStd (gzReadFilePS . toFilePath) gzReadStdin patchesfile
  let from_whom = getFrom ps
  us <- readRepo repository
  either_them <- getPatchBundle opts ps
  Sealed them
     <- case either_them of
          Right t -> return t
          Left er -> do forwarded <- considerForwarding opts ps
                        if forwarded
                          then exitWith ExitSuccess
                          else fail er
  common :> _ <- return $ findCommonWithThem us them

  -- all patches that are in "them" and not in "common" need to be available; check that
  let common_i = mapRL info $ newset2RL common
      them_i = mapRL info $ newset2RL them
      required = them_i \\ common_i -- FIXME quadratic?
      check :: RL (PatchInfoAnd p) C(x y) -> [PatchInfo] -> IO ()
      check (p :<: ps') bad = case hopefullyM p of
        Nothing | info p `elem` required -> check ps' (info p : bad)
        _ -> check ps' bad
      check NilRL [] = return ()
      check NilRL bad = fail . renderString $ vcat $ map humanFriendly bad ++
                        [ text "\nFATAL: Cannot apply this bundle. We are missing the above patches." ]

  check (newset2RL them) []

  (us':\/:them') <- return $ findUncommon us them
  (hadConflicts, Sealed their_ps) <- filterOutConflicts opts (reverseFL us') repository them'
  when hadConflicts $ putStrLn "Skipping some patches which would cause conflicts."
  when (nullFL their_ps) $
       do putStr $ "All these patches have already been applied.  " ++
                     "Nothing to do.\n"
          exitWith ExitSuccess
  let context = selectionContext "apply" fixed_opts Nothing []
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  (to_be_applied :> _) <- runSelection (selector their_ps) context
  applyItNow opts from_whom repository us' to_be_applied
    where fixed_opts = if Interactive `elem` opts
                         then opts
                         else All : opts
applyCmd _ _ = impossible

applyItNow :: FORALL(p r u t x z) RepoPatch p =>
             [DarcsFlag] -> String -> Repository p C(r u t)
           -> FL (PatchInfoAnd p) C(x t) -> FL (PatchInfoAnd p) C(x z) -> IO ()
applyItNow opts from_whom repository us' to_be_applied = do
   printDryRunMessageAndExit "apply" opts to_be_applied
   when (nullFL to_be_applied) $
        do putStrLn "You don't want to apply any patches, so I'm exiting!"
           exitWith ExitSuccess
   checkPaths opts to_be_applied
   redirectOutput opts from_whom $ do
    putVerbose opts $ text "Will apply the following patches:"
    putVerbose opts . vcat $ mapFL description to_be_applied
    setEnvDarcsPatches to_be_applied
    Sealed pw <- tentativelyMergePatches repository "apply" opts us' to_be_applied
    invalidateIndex repository
    rc <- testTentative repository
    when (rc /= ExitSuccess) $ do
        when (not $ isInteractive opts) $ exitWith rc
        putStrLn $ "Looks like those patches do not pass the tests."
        let prompt = "Shall I apply them anyway?"
        yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
        case yn of
          'y' -> return ()
          _ -> exitWith rc
    withSignalsBlocked $ do finalizeRepositoryChanges repository
                            _ <- applyToWorking repository opts pw `catch` \(e :: SomeException) ->
                                fail ("Error applying patch to working dir:\n" ++ show e)
                            makeScriptsExecutable opts pw
                            return ()
    putStrLn "Finished applying..."
\end{code}

Darcs apply accepts a single argument, which is the name of the patch
file to be applied.  If you omit this argument, the patch is read from
standard input.  Darcs also interprets an argument of `\-' to mean it
should read the file from standard input. This allows you to use apply
with a pipe from your email program, for example.

\begin{options}
--verify
\end{options}

\begin{code}

getPatchBundle :: RepoPatch p => [DarcsFlag] -> B.ByteString
                 -> IO (Either String (SealedPatchSet p C(Origin)))
getPatchBundle opts fps = do
    mps <- verifyPS opts $ readEmail fps
    mops <- verifyPS opts fps
    case (mps, mops) of
      (Nothing, Nothing) ->
          return $ Left "Patch bundle not properly signed, or gpg failed."
      (Just ps, Nothing) -> return $ scanBundle ps
      (Nothing, Just ps) -> return $ scanBundle ps
      -- We use careful_scan_bundle only below because in either of the two
      -- above case we know the patch was signed, so it really shouldn't
      -- need stripping of CRs.
      (Just ps1, Just ps2) -> case careful_scan_bundle ps1 of
                              Left _ -> return $ careful_scan_bundle ps2
                              Right x -> return $ Right x
          where careful_scan_bundle ps =
                    case scanBundle ps of
                    Left e -> case scanBundle $ stripCrPS ps of
                              Right x -> Right x
                              _ -> Left e
                    x -> x
                stripCrPS :: B.ByteString -> B.ByteString
                stripCrPS ps = unlinesPS $ map stripline $ linesPS ps
                stripline p | B.null p = p
                            | BC.last p == '\r' = B.init p
                            | otherwise = p

applyHelp' :: String
applyHelp' =
 "A patch bundle may introduce unresolved conflicts with existing\n" ++
 "patches or with the working tree.  By default, Darcs will add conflict\n" ++
 "markers (see `darcs mark-conflicts').\n" ++
 "\n" ++
 "The --allow-conflicts option will skip conflict marking; this is\n" ++
 "useful when you want to treat a repository as just a bunch of patches,\n" ++
 "such as using `darcs pull --union' to download of your co-workers\n" ++
 "patches before going offline.\n" ++
 "\n" ++
 "This can mess up unrecorded changes in the working tree, forcing you\n" ++
 "to resolve the conflict immediately.  To simply reject bundles that\n" ++
 "introduce unresolved conflicts, using the --dont-allow-conflicts\n" ++
 "option.  Making this the default in push-based workflows is strongly\n" ++
 "recommended.\n" ++
 "\n" ++
 "Unlike most Darcs commands, `darcs apply' defaults to --all.  Use the\n" ++
 "--interactive option to pick which patches to apply from a bundle.\n"

\end{code}
\begin{options}
--external-merge
\end{options}

You can use an external interactive merge tool to resolve conflicts with the
flag \verb!--external-merge!.  For more details see
subsection~\ref{resolution}.

\begin{options}
--sendmail-command
\end{options}

If you want to use a command different from the default one for sending mail,
you need to specify a command line with the \verb!--sendmail-command! option.
The command line can contain the format specifier \verb!%t! for to
and you can add \verb!%<! to the end of the command line if the command
expects the complete mail on standard input. For example, the command line for
msmtp looks like this:

\begin{verbatim}
msmtp -t %<
\end{verbatim}


\begin{code}
getFrom :: B.ByteString -> String
getFrom ps = readFrom $ linesPS ps
    where readFrom [] = ""
          readFrom (x:xs)
           | B.take 5 x == fromStart = BC.unpack $ B.drop 5 x
           | otherwise = readFrom xs

redirectOutput :: forall a . [DarcsFlag] -> String -> IO a -> IO a
redirectOutput opts to doit = ro opts
    where
  cc = getCc opts
  ro [] = doit
  ro (Reply f:_) =
    withStdoutTemp $ \tempf-> do {a <- doit;
                                  hClose stdout;
                                  hClose stderr;
                                  return a;
                                 } `catch` (sendit tempf)
        where sendit :: FilePath -> SomeException -> IO a
              sendit tempf e | Just ExitSuccess <- fromException e =
                do sendSanitizedEmail opts f to "Patch applied" cc tempf
                   throwIO e
              sendit tempf e | Just (_ :: ExitCode) <- fromException e =
                do sendSanitizedEmail opts f to "Patch failed!" cc tempf
                   throwIO ExitSuccess
              sendit tempf e =
                do sendSanitizedEmail opts f to "Darcs error applying patch!" cc $
                             tempf ++ "\n\nCaught exception:\n"++
                             show e++"\n"
                   throwIO ExitSuccess
  ro (_:fs) = ro fs

-- |sendSanitizedEmail sends a sanitized email using the given sendmailcmd
-- It takes @DacrsFlag@ options a file with the mail contents,
-- To:, Subject:, CC:, and mail body
sendSanitizedEmail :: [DarcsFlag] -> String -> String -> String -> String -> String -> IO ()
sendSanitizedEmail opts file to subject cc mailtext =
    do scmd <- getSendmailCmd opts
       body <- sanitizeFile mailtext
       sendEmail file to subject cc scmd body

-- sanitizeFile is used to clean up the stdout/stderr before sticking it in
-- an email.

sanitizeFile :: FilePath -> IO String
sanitizeFile f = sanitize `fmap` readBinFile f
    where sanitize s = wash $ remove_backspaces "" s
          wash ('\000':s) = "\\NUL" ++ wash s
          wash ('\026':s) = "\\EOF" ++ wash s
          wash (c:cs) = c : wash cs
          wash [] = []
          remove_backspaces rev_sofar "" = reverse rev_sofar
          remove_backspaces (_:rs) ('\008':s) = remove_backspaces rs s
          remove_backspaces "" ('\008':s) = remove_backspaces "" s
          remove_backspaces rs (s:ss) = remove_backspaces (s:rs) ss

forwardingMessage :: B.ByteString
forwardingMessage = BC.pack $
    "The following patch was either unsigned, or signed by a non-allowed\n"++
    "key, or there was a GPG failure.\n"

considerForwarding :: [DarcsFlag] -> B.ByteString -> IO Bool
considerForwarding opts m = cf opts (getCc opts)
    where cf [] _ = return False
          cf (Reply t:_) cc =
              case break is_from (linesPS m) of
              (m1, f:m2) ->
                  let m_lines = forwardingMessage:m1 ++ m2
                      m' = unlinesPS m_lines
                      f' = BC.unpack (B.drop 5 f) in
                      if t == f' || t == init f'
                      then return False -- Refuse possible email loop.
                      else do
                        scmd <- getSendmailCmd opts
                        if doHappyForwarding opts
                         then resendEmail t scmd m
                         else sendEmailDoc f' t "A forwarded darcs patch" cc
                                           scmd (Just (empty,empty))
                                           (packedString m')
                        return True
              _ -> return False -- Don't forward emails lacking headers!
          cf (_:fs) cc = cf fs cc
          is_from l = B.take 5 l == fromStart

fromStart :: B.ByteString
fromStart = BC.pack "From:"
\end{code}
