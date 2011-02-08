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

\darcsCommand{send}
\begin{code}
{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.Commands.Send ( send ) where
import System.Exit ( exitWith, ExitCode( ExitSuccess ) )
import System.IO.Error ( ioeGetErrorString )
import System.IO ( hClose )
import Control.Monad ( when, unless, forM_ )
import Storage.Hashed.Tree ( Tree )
import Data.Maybe ( isNothing )

import Darcs.Commands ( DarcsCommand(..), putInfo, putVerbose )
import Darcs.Arguments ( DarcsFlag( EditDescription, LogFile,
                                    Target, Context,
                                    DryRun, Quiet
                                  ), getOutput,
                         fixUrl, setEnvDarcsPatches,
                         getCc, getAuthor, workingRepoDir,
                         editDescription, logfile, rmlogfile,
                         sign, getSubject, depsSel, getInReplyTo,
                         matchSeveral, setDefault, outputAutoName,
                         output, ccSend, subject, target, author, sendmailCmd,
                         inReplyTo, remoteRepo, networkOptions,
                         allInteractive, getSendmailCmd,
                         printDryRunMessageAndExit,
                         summary, allowUnrelatedRepos,
                         fromOpt, dryRun, sendToContext, getOutput,
                         changesReverse,
                       )
import Darcs.Flags ( willRemoveLogFile, doReverse )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, patchDesc )
import Darcs.Repository ( PatchSet, Repository,
                          amInRepository, identifyRepositoryFor, withRepoReadLock, RepoJob(..),
                          readRepo, readRecorded, prefsUrl, checkUnrelatedRepos )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Patch ( RepoPatch, description, applyToTree, invert )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (:\/:)(..), (:>)(..),
                       mapFL, mapFL_FL, lengthFL, nullFL )
import Darcs.Patch.Bundle ( makeBundleN, scanContext, patchFilename )
import Darcs.Repository.Prefs ( defaultrepo, setDefaultrepo, getPreflist )
import Darcs.External ( signString, sendEmailDoc, fetchFilePS, Cachable(..), generateEmail )
import ByteStringUtils ( mmapFilePS )
import qualified Data.ByteString.Char8 as BC (unpack)
import Darcs.Lock ( withOpenTemp, writeDocBinFile, readDocBinFile, worldReadableTemp, removeFileMayNotExist )
import Darcs.SelectChanges ( selectChanges, WhichChanges(..), selectionContext, runSelection )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Utils ( askUser, promptYorn, catchall, editFile, formatPath )
import Progress ( debugMessage )
import Darcs.Email ( makeEmail )
import Printer ( Doc, vsep, vcat, text, ($$), (<+>), (<>), putDoc )
import Darcs.RepoPath ( FilePathLike, toFilePath, AbsolutePath, AbsolutePathOrStd,
                        getCurrentDirectory, useAbsoluteOrStd )
import HTTP ( postUrl )
#include "impossible.h"

#include "gadts.h"

sendDescription :: String
sendDescription =
 "Send by email a bundle of one or more patches."

sendHelp :: String
sendHelp =
 "Send is used to prepare a bundle of patches that can be applied to a target\n"++
 "repository.  Send accepts the URL of the repository as an argument.  When\n"++
 "called without an argument, send will use the most recent repository that\n"++
 "was either pushed to, pulled from or sent to.  By default, the patch bundle\n"++
 "is sent by email, although you may save it to a file.\n"
\end{code}

Do not confuse the \verb!--author! options with the return address
that \verb!darcs send! will set for your patch bundle.

For example, if you have two email addresses A and B:
\begin{description}
\item  If you use
\verb!--author A! but your machine is configured to send mail from
address B by default, then the return address on your message will be B.

\item If you use \verb!--from A! and your mail client supports setting the
From: address arbitrarily (some non-Unix-like mail clients, especially,
may not support this), then the return address will be A; if it does
not support this, then the return address will be B.

\item If you supply neither \verb!--from! nor \verb!--author!, then the return
address will be B.
\end{description}

In addition, unless you specify the sendmail command with
\verb!--sendmail-command!, darcs sends email using the default email
command on your computer. This default command is determined by the
\verb!configure! script. Thus, on some non-Unix-like OSes,
\verb!--from! is likely to not work at all.

\begin{code}
send :: DarcsCommand
send = DarcsCommand {commandProgramName = "darcs",
                     commandName = "send",
                     commandHelp = sendHelp,
                     commandDescription = sendDescription,
                     commandExtraArgs = 1,
                     commandExtraArgHelp = ["[REPOSITORY]"],
                     commandCommand = sendCmd,
                     commandPrereq = amInRepository,
                     commandGetArgPossibilities = getPreflist "repos",
                     commandArgdefaults = defaultrepo,
                     commandAdvancedOptions = [logfile, rmlogfile,
                                                 remoteRepo,
                                                 sendToContext, changesReverse] ++
                                                networkOptions,
                     commandBasicOptions = [matchSeveral, depsSel,
                                              allInteractive,
                                              fromOpt, author,
                                              target,ccSend,subject, inReplyTo,
                                              output,outputAutoName,sign]
                                              ++dryRun++[summary,
                                              editDescription,
                                              setDefault False,
                                              workingRepoDir,
                                              sendmailCmd,
                                              allowUnrelatedRepos]}

sendCmd :: [DarcsFlag] -> [String] -> IO ()
sendCmd input_opts [""] = sendCmd input_opts []
sendCmd input_opts [unfixedrepodir] = withRepoReadLock input_opts $ RepoJob $ \repository -> do
  context_ps <- the_context input_opts
  case context_ps of
    Just them -> sendToThem repository input_opts [] "CONTEXT" them
    Nothing -> do
        repodir <- fixUrl input_opts unfixedrepodir
        -- Test to make sure we aren't trying to push to the current repo
        here <- getCurrentDirectory
        when (repodir == toFilePath here) $
           fail ("Can't send to current repository! Did you mean send --context?")
        old_default <- getPreflist "defaultrepo"
        when (old_default == [repodir] && not (Quiet `elem` input_opts)) $
             putStrLn $ "Creating patch to "++formatPath repodir++"..."
        repo <- identifyRepositoryFor repository repodir
        them <- readRepo repo
        setDefaultrepo repodir input_opts
        wtds <- decideOnBehavior input_opts repo
        sendToThem repository input_opts wtds repodir them
    where the_context [] = return Nothing
          the_context (Context foo:_)
              = (Just . scanContext )`fmap` mmapFilePS (toFilePath foo)
          the_context (_:fs) = the_context fs
sendCmd _ _ = impossible

sendToThem :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> [WhatToDo] -> String -> PatchSet p C(Origin x) -> IO ()
sendToThem repo opts wtds their_name them = do
  us <- readRepo repo
  common :> us' <- return $ findCommonWithThem us them
  checkUnrelatedRepos opts us them
  (case us' of
      NilFL -> do putInfo opts $ text "No recorded local changes to send!"
                  exitWith ExitSuccess
      _ -> putVerbose opts $ text "We have the following patches to send:"
                     $$ (vcat $ mapFL description us')) :: IO ()
  pristine <- readRecorded repo
  let context = selectionContext "send" opts Nothing []
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  (to_be_sent :> _) <- runSelection (selector us') context
  printDryRunMessageAndExit "send" opts to_be_sent
  when (nullFL to_be_sent) $ do
      putInfo opts $ text "You don't want to send any patches, and that's fine with me!"
      exitWith ExitSuccess
  setEnvDarcsPatches to_be_sent
  bundle <- prepareBundle opts common pristine (us':\/:to_be_sent)
  let make_fname (tb:>:_) = patchFilename $ patchDesc tb
      make_fname _ = impossible
      fname = make_fname to_be_sent
      outname = getOutput opts fname
  case outname of
    Just fname' -> writeBundleToFile opts to_be_sent bundle fname' wtds their_name
    Nothing -> sendBundle opts to_be_sent bundle fname wtds their_name

prepareBundle :: forall p C(x y z). RepoPatch p => [DarcsFlag] -> PatchSet p C(Origin z)
                -> Tree IO -> ((FL (PatchInfoAnd p)) :\/: (FL (PatchInfoAnd p))) C(x y)
                -> IO Doc
prepareBundle opts common pristine (us' :\/: to_be_sent) = do
  pristine' <- applyToTree (invert $ mapFL_FL hopefully us') pristine
  unsig_bundle <- makeBundleN (Just pristine') (unsafeCoerceP common) (mapFL_FL hopefully to_be_sent)
  signString opts unsig_bundle

sendBundle :: forall p C(x y) . (RepoPatch p) => [DarcsFlag] -> FL (PatchInfoAnd p) C(x y)
             -> Doc -> String -> [WhatToDo] -> String -> IO ()
sendBundle opts to_be_sent bundle fname wtds their_name=
         let
           auto_subject :: forall pp C(a b) . FL (PatchInfoAnd pp) C(a b) -> String
           auto_subject (p:>:NilFL)  = "darcs patch: " ++ trim (patchDesc p) 57
           auto_subject (p:>:ps) = "darcs patch: " ++ trim (patchDesc p) 43 ++
                            " (and " ++ show (lengthFL ps) ++ " more)"
           auto_subject _ = error "Tried to get a name from empty patch list."
           trim st n = if length st <= n then st
                       else take (n-3) st ++ "..."
           in do
           thetargets <- getTargets wtds
           from <- getAuthor opts
           let thesubject = case getSubject opts of
                            Nothing -> auto_subject to_be_sent
                            Just subj -> subj
           (mailcontents, mailfile) <- getDescription opts their_name to_be_sent
           let body = makeEmail their_name
                        (maybe [] (\x -> [("In-Reply-To", x), ("References", x)]) . getInReplyTo $ opts)
                        (Just mailcontents)
                        bundle
                        (Just fname)
               contentAndBundle = Just (mailcontents, bundle)

               sendmail = do
                 sm_cmd <- getSendmailCmd opts
                 (sendEmailDoc from (lt [t | SendMail t <- thetargets]) (thesubject) (getCc opts)
                               sm_cmd contentAndBundle body >>
                  (putInfo opts . text $ ("Successfully sent patch bundle to: "
                            ++ lt [ t | SendMail t <- thetargets ]
                            ++ ccs (getCc opts) ++".")))
                 `catch` \e -> let msg = "Email body left in " in
                               do case mailfile of
                                    Just mf -> putStrLn $ msg++mf++"."
                                    Nothing -> return ()
                                  fail $ ioeGetErrorString e
               ccs [] = []
               ccs cs  = " and cc'ed " ++ cs

           when (null [ p | Post p <- thetargets]) sendmail
           nbody <- withOpenTemp $ \ (fh,fn) -> do
               generateEmail fh from (lt [t | SendMail t <- thetargets]) thesubject (getCc opts) body
               hClose fh
               mmapFilePS fn
           forM_ [ p | Post p <- thetargets]
             (\url -> do
                putInfo opts . text $ "Posting patch to " ++ url
                postUrl url (BC.unpack nbody) "message/rfc822")
             `catch` const sendmail
           cleanup opts mailfile


lt :: [String] -> String
lt [t] = t
lt [t,""] = t
lt (t:ts) = t++" , "++lt ts
lt [] = ""

cleanup :: (FilePathLike t) => [DarcsFlag] -> Maybe t -> IO ()
cleanup opts (Just mailfile) = when (isNothing (getFileopt opts) || (willRemoveLogFile opts)) $
                                      removeFileMayNotExist mailfile
cleanup _ Nothing = return ()

writeBundleToFile :: forall p C(x y) . (RepoPatch p) => [DarcsFlag] -> FL (PatchInfoAnd p) C(x y) -> Doc ->
                    AbsolutePathOrStd -> [WhatToDo] -> String -> IO ()
writeBundleToFile opts to_be_sent bundle fname wtds their_name =
    do (d,f) <- getDescription opts their_name to_be_sent
       let putabs a = do writeDocBinFile a (d $$ bundle)
                         putStrLn $ "Wrote patch to " ++ toFilePath a ++ "."
           putstd = putDoc (d $$ bundle)
       useAbsoluteOrStd putabs putstd fname
       let mails = lt [ t | SendMail t <- wtds ]
       unless (null mails) $ putInfo opts $ text $ "The usual recipent for this bundle is: " ++ mails
       cleanup opts f
\end{code}

\begin{options}
--output, --to, --cc
\end{options}

The \verb!--output!, \verb!--output-auto-name!, and \verb!--to! flags determine
what darcs does with the patch bundle after creating it.  If you provide an
\verb!--output!  argument, the patch bundle is saved to that file.  If you
specify \verb!--output-auto-name!, the patch bundle is saved to a file with an
automatically generated name.  If you give one or more \verb!--to! arguments,
the bundle of patches is sent to those locations. The locations may either be email
addresses or urls that the patch should be submitted to via HTTP.

If you don't provide any of these options, darcs will look at the contents of
the \verb!_darcs/prefs/email! file in the target repository (if it exists), and
send the patch by email to that address.  In this case, you may use the
\verb!--cc! option to specify additional recipients without overriding the
default repository email address.

If \texttt{\_darcs/prefs/post} exists in the target repository, darcs will
upload to the URL contained in that file, which may either be a
\texttt{mailto:} URL, or an \texttt{http://} URL.  In the latter case, the
patch is posted to that URL.

If there is no email address associated with the repository, darcs will
prompt you for an email address.

\begin{options}
--subject
\end{options}

Use the \verb!--subject! flag to set the subject of the e-mail to be sent.
If you don't provide a subject on the command line, darcs will make one up
based on names of the patches in the patch bundle.

\begin{options}
--in-reply-to
\end{options}

Use the \verb!--in-reply-to! flag to set the In-Reply-To and References headers
of the e-mail to be sent. By default no additional headers are included so e-mail
will not be treated as reply by mail readers.

\begin{code}

data WhatToDo
    = Post String        -- ^ POST the patch via HTTP
    | SendMail String    -- ^ send patch via email


decideOnBehavior :: RepoPatch p => [DarcsFlag] -> Repository p C(r u t) -> IO [WhatToDo]
decideOnBehavior opts the_remote_repo =
    case the_targets of
    [] -> do wtds <- check_post
             unless (null wtds) $ announce_recipients wtds
             return wtds
    ts -> do announce_recipients ts
             return ts
    where the_targets = collectTargets opts
#ifdef HAVE_HTTP
          -- the ifdef above is to so that darcs only checks the remote
          -- _darcs/post if we have an implementation of postUrl.  See
          -- our HTTP module for more details
          check_post = do p <- ((readPost . BC.unpack) `fmap`
                                fetchFilePS (prefsUrl the_remote_repo++"/post")
                                (MaxAge 600)) `catchall` return []
                          emails <- who_to_email
                          return (p++emails)
          readPost p = map pp (lines p) where
            pp ('m':'a':'i':'l':'t':'o':':':s) = SendMail s
            pp s = Post s
#else
          check_post = who_to_email
#endif
          who_to_email =
              do email <- (BC.unpack `fmap`
                           fetchFilePS (prefsUrl the_remote_repo++"/email")
                                       (MaxAge 600))
                          `catchall` return ""
                 if '@' `elem` email then return . map SendMail $ lines email
                                     else return []
          announce_recipients emails =
            let pn (SendMail s) = s
                pn (Post p) = p
            in if DryRun `elem` opts
            then putInfo opts . text $ "Patch bundle would be sent to: "++unwords (map pn emails)
            else when (null the_targets) $
                 putInfo opts . text $ "Patch bundle will be sent to: "++unwords (map pn emails)

getTargets :: [WhatToDo] -> IO [WhatToDo]
getTargets [] = do fmap ((:[]) . SendMail) $ askUser "What is the target email address? "
getTargets wtds = return wtds

collectTargets :: [DarcsFlag] -> [WhatToDo]
collectTargets flags = [ f t | Target t <- flags ] where
    f url@('h':'t':'t':'p':':':_) = Post url
    f em = SendMail em


\end{code}

\begin{options}
--matches, --patches, --tags, --no-deps
\end{options}

The \verb!--patches!, \verb!--matches!, \verb!--tags!, and \verb!--no-deps!
options can be used to select which patches to send, as described in
subsection~\ref{selecting}.

\begin{options}
--edit-description
\end{options}

If you want to include a description or explanation along with the bundle
of patches, you need to specify the \verb!--edit-description! flag, which
will cause darcs to open up an editor with which you can compose a message
to go along with your patches.

\begin{options}
--sendmail-command
\end{options}

If you want to use a command different from the default one for sending email,
you need to specify a command line with the \verb!--sendmail-command! option. The
command line can contain some format specifiers which are replaced by the actual
values. Accepted format specifiers are \verb!%s! for subject, \verb!%t! for to,
\verb!%c! for cc, \verb!%b! for the body of the mail, \verb!%f! for from, \verb!%a!
for the patch bundle and the same specifiers in uppercase for the URL-encoded values.
Additionally you can add \verb!%<! to the end of the command line if the command
expects the complete email message on standard input. E.g.\ the command lines for evolution
and msmtp look like this:

\begin{verbatim}
evolution "mailto:%T?subject=%S&attach=%A&cc=%C&body=%B"
msmtp -t %<
\end{verbatim}

\begin{code}
getDescription :: RepoPatch p => [DarcsFlag] -> String -> FL (PatchInfoAnd p) C(x y) -> IO (Doc, Maybe String)
getDescription opts their_name patches =
    case get_filename of
        Just f -> do file <- f
                     when (EditDescription `elem` opts) $ do
                       when (isNothing $ getFileopt opts) $
                            writeDocBinFile file patchdesc
                       debugMessage $ "About to edit file " ++ file
                       (_, changed) <- editFile file
                       unless changed $ do
                         yorn <- promptYorn "File content did not change. Continue anyway?"
                         when (yorn == 'n') $ do putStrLn "Aborted."
                                                 exitWith ExitSuccess
                       return ()
                     doc <- readDocBinFile file
                     return (doc, Just file)
        Nothing -> return (patchdesc, Nothing)
    where patchdesc = text (if lengthFL patches == 1
                               then "1 patch"
                               else show (lengthFL patches) ++ " patches")
                      <+> text "for repository" <+> text their_name <> text ":"
                      $$ text ""
                      $$ vsep (mapFL description patches)
          get_filename = case getFileopt opts of
                                Just f -> Just $ return $ toFilePath f
                                Nothing -> if EditDescription `elem` opts
                                              then Just tempfile
                                              else Nothing
          tempfile = worldReadableTemp "darcs-temp-mail"

getFileopt :: [DarcsFlag] -> Maybe AbsolutePath
getFileopt (LogFile f:_) = Just f
getFileopt (_:flags) = getFileopt flags
getFileopt [] = Nothing
\end{code}
