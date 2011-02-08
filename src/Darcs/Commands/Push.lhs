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

\darcsCommand{push}
\begin{code}
{-# LANGUAGE CPP, TypeOperators #-}

module Darcs.Commands.Push ( push ) where
import System.Exit ( exitWith, ExitCode( ExitSuccess, ExitFailure ) )
import Control.Monad ( when )
import Data.Char ( toUpper )
import Workaround ( getCurrentDirectory )
import Darcs.Commands ( DarcsCommand(..), putVerbose, putInfo, abortRun )
import Darcs.Arguments ( DarcsFlag( DryRun, Sign, SignAs, NoSign, SignSSL ),
                         setEnvDarcsPatches,
                         workingRepoDir, summary,
                         printDryRunMessageAndExit,
                         applyas, matchSeveral, fixUrl, depsSel,
                         allInteractive, dryRun,
                         remoteRepo, networkOptions,
                         setDefault, sign, allowUnrelatedRepos,
                         changesReverse
                      )

import Darcs.Flags(doReverse)
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully )
import Darcs.Repository ( Repository, withRepoReadLock, RepoJob(..), identifyRepositoryFor,
                          readRepo, amInRepository, checkUnrelatedRepos )
import Darcs.Patch ( RepoPatch, description )
import Darcs.Witnesses.Ordered ( (:>)(..), RL, FL, nullRL,
                             nullFL, reverseFL, mapFL_FL, mapRL )
import Darcs.Repository.Prefs ( defaultrepo, setDefaultrepo, getPreflist )
import Darcs.External ( maybeURLCmd, signString )
import Darcs.URL ( isHttpUrl, isFile )
import Darcs.SelectChanges ( selectChanges, WhichChanges(..),
                             selectionContext, runSelection )
import Darcs.Utils ( formatPath )
import Darcs.Patch.Depends ( findCommonWithThem, countUsThem )
import Darcs.Patch.Bundle ( makeBundleN )
import Darcs.Patch.Patchy( ShowPatch )
import Darcs.Patch.Set ( PatchSet )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Printer ( Doc, vcat, empty, text, ($$) )
import Darcs.RemoteApply ( remoteApply, applyAs )
import Darcs.Email ( makeEmail )
import English (englishNum, Noun(..))
#include "impossible.h"

#include "gadts.h"

pushDescription :: String
pushDescription =
 "Copy and apply patches from this repository to another one."

pushHelp :: String
pushHelp =
 "Push is the opposite of pull.  Push allows you to copy changes from the\n"++
 "current repository into another repository.\n"

push :: DarcsCommand
push = DarcsCommand {commandProgramName = "darcs",
                     commandName = "push",
                     commandHelp = pushHelp,
                     commandDescription = pushDescription,
                     commandExtraArgs = 1,
                     commandExtraArgHelp = ["[REPOSITORY]"],
                     commandCommand = pushCmd,
                     commandPrereq = amInRepository,
                     commandGetArgPossibilities = getPreflist "repos",
                     commandArgdefaults = defaultrepo,
                     commandAdvancedOptions = [applyas,
                                                 remoteRepo,
                                                 changesReverse] ++
                                                networkOptions,
                     commandBasicOptions = [matchSeveral, depsSel,
                                              allInteractive,
                                              sign]++dryRun++[summary,
                                              workingRepoDir,
                                              setDefault False,
                                              allowUnrelatedRepos]}

pushCmd :: [DarcsFlag] -> [String] -> IO ()
pushCmd _ [""] = impossible
pushCmd opts [unfixedrepodir] =
 do
 repodir <- fixUrl opts unfixedrepodir
 -- Test to make sure we aren't trying to push to the current repo
 here <- getCurrentDirectory
 checkOptionsSanity opts repodir
 when (repodir == here) $
       fail "Cannot push from repository to itself."
       -- absolute '.' also taken into account by fix_filepath
 (bundle) <- withRepoReadLock opts $ RepoJob $
                          prepareBundle opts repodir
 sbundle <- signString opts bundle
 let body = if isFile repodir
            then sbundle
            else makeEmail repodir [] Nothing sbundle Nothing
 rval <- remoteApply opts repodir body
 case rval of ExitFailure ec -> do putStrLn $ "Apply failed!"
                                   exitWith (ExitFailure ec)
              ExitSuccess -> putInfo opts $ text "Push successful."
pushCmd _ _ = impossible

prepareBundle :: forall p C(r u t) . (RepoPatch p) => [DarcsFlag] -> String -> Repository p C(r u t) ->
                IO (Doc)
prepareBundle opts repodir repository = do
  old_default <- getPreflist "defaultrepo"
  when (old_default == [repodir]) $
       let pushing = if DryRun `elem` opts then "Would push" else "Pushing"
       in  putInfo opts $ text $ pushing++" to "++formatPath repodir++"..."
  them <- identifyRepositoryFor repository repodir >>= readRepo
  setDefaultrepo repodir opts
  us <- readRepo repository
  common :> us' <- return $ findCommonWithThem us them
  prePushChatter opts us (reverseFL us') them
  let context = selectionContext "push" opts Nothing []
      selector = if doReverse opts
                 then selectChanges FirstReversed
                 else selectChanges First
  runSelection (selector us') context
                   >>= bundlePatches opts common

prePushChatter :: forall p a C(x y t) . (RepoPatch p, ShowPatch a) =>
                 [DarcsFlag] -> PatchSet p C(Origin x) ->
                 RL a C(t x) -> PatchSet p C(Origin y) -> IO ()
prePushChatter opts us us' them = do
  checkUnrelatedRepos opts us them
  let num_to_pull = snd $ countUsThem us them
      pull_reminder = if num_to_pull > 0
                      then text $ "The remote repository has " ++ show num_to_pull
                      ++ " " ++ englishNum num_to_pull (Noun "patch") " to pull."
                      else empty
  putVerbose opts $ text "We have the following patches to push:" $$ (vcat $ mapRL description us')
  when (not $ nullRL us') $ do putInfo opts $ pull_reminder
  when (nullRL us') $ do putInfo opts $ text "No recorded local changes to push!"
                         exitWith ExitSuccess

bundlePatches :: forall t p C(z w a). RepoPatch p => [DarcsFlag] -> PatchSet p C(a z)
                                      -> (FL (PatchInfoAnd p) :> t) C(z w)
                                      -> IO (Doc)
bundlePatches opts common (to_be_pushed :> _) =
    do
      setEnvDarcsPatches to_be_pushed
      printDryRunMessageAndExit "push" opts to_be_pushed
      when (nullFL to_be_pushed) $ do
          putInfo opts $
            text "You don't want to push any patches, and that's fine with me!"
          exitWith ExitSuccess
      bundle <- makeBundleN Nothing
                     common (mapFL_FL hopefully to_be_pushed)
      return (bundle)

wantSign :: [DarcsFlag] -> Bool
wantSign opts = case opts of
    []            -> False
    Sign:_        -> True
    (SignAs _):_  -> True
    (SignSSL _):_ -> True
    NoSign:_      -> False
    _:opts'       -> wantSign opts'


checkOptionsSanity :: [DarcsFlag] -> String -> IO ()
checkOptionsSanity opts repodir =
  if isHttpUrl repodir then do
       when (applyAs opts /= Nothing) $
           abortRun opts $ text "Cannot --apply-as when pushing to URLs"
       maybeapply <- maybeURLCmd "APPLY" repodir
       when (maybeapply == Nothing) $
         let lprot = takeWhile (/= ':') repodir
             prot = map toUpper lprot
             msg = text ("Pushing to "++lprot++" URLs is not supported.\n"++
                         "You may be able to hack this to work"++
                         " using DARCS_APPLY_"++prot) in
         abortRun opts msg
   else do
       when (wantSign opts) $
        abortRun opts $ text "Signing doesn't make sense for local repositories or when pushing over ssh."

\end{code}

For obvious reasons, you can only push to repositories to which you have
write access.  In addition, you can only push to repos that you access
either on the local file system or with ssh.  In order to apply with ssh,
darcs must also be installed on the remote computer.  The command invoked
to run ssh may be configured by the \verb!DARCS_SSH! environment variable
(see subsection~\ref{env:DARCS_SSH}).  The command invoked via ssh is always
\verb!darcs!, i.e.\ the darcs executable must be in the default path on
the remote machine.

Push works by creating a patch bundle, and then running darcs apply in the
target repository using that patch bundle.  This means that the default
options for \emph{apply} in the \emph{target} repository (such as, for
example, \verb!--test!) will affect the behavior of push.  This also means
that push is somewhat less efficient than pull.

When you receive an error message such as
\begin{verbatim}
bash: darcs: command not found
\end{verbatim}
then this means that the darcs on the remote machine could
not be started.  Make sure that the darcs executable is called
\verb!darcs! and is found in the default path.  The default path can
be different in interactive and in non-interactive shells.  Say
\begin{verbatim}
ssh login@remote.machine darcs
\end{verbatim}
to try whether the remote darcs can be found, or
\begin{verbatim}
ssh login@remote.machine 'echo $PATH'
\end{verbatim}
(note the single quotes) to check the default path.

\begin{options}
--apply-as
\end{options}

If you give the \verb!--apply-as! flag, darcs will use sudo to apply the
changes as a different user.  This can be useful if you want to set up a
system where several users can modify the same repository, but you don't
want to allow them full write access.  This isn't secure against skilled
malicious attackers, but at least can protect your repository from clumsy,
inept or lazy users.

\begin{options}
--matches, --patches, --tags, --no-deps
\end{options}

The \verb!--patches!, \verb!--matches!, \verb!--tags!, and \verb!--no-deps!
options can be used to select which patches to push, as described in
subsection~\ref{selecting}.

When there are conflicts, the behavior of push is determined by the default
flags to \verb!apply! in the \emph{target} repository.  Most commonly, for
pushed-to repositories, you'd like to have \verb!--dont-allow-conflicts! as
a default option to apply (by default, it is already the default\ldots).  If
this is the case, when there are conflicts on push, darcs will fail with an
error message.  You can then resolve by pulling the conflicting patch,
recording a resolution and then pushing the resolution together with the
conflicting patch.

Darcs does not have an explicit way to tell you which patch conflicted, only the
file name. You may want to pull all the patches from the remote repository just
to be sure. If you don't want to do this in your working directory,
you can create another darcs working directory for this purpose.

If you want, you could set the target repository to use \verb!--allow-conflicts!.
In this case conflicting patches will be applied, but the conflicts will
not be marked in the working directory.

If, on the other hand, you have \verb!--mark-conflicts! specified as a
default flag for apply in the target repository, when there is a conflict,
it will be marked in the working directory of the target repository.  In
this case, you should resolve the conflict in the target repository itself.
