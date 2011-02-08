%  Copyright (C) 2002-2005 David Roundy
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

\darcsCommand{unrecord}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Unrecord ( unrecord, unpull, obliterate, getLastPatches ) where
import Control.Monad ( when )
import System.Exit ( exitWith, ExitCode( ExitSuccess ) )
import Data.Maybe( isJust )

import Printer ( text, putDoc )
import English ( presentParticiple )
import Darcs.Patch.PatchInfoAnd ( hopefully, patchDesc )
import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias,
                        putVerbose )
import Darcs.Arguments ( DarcsFlag,
                         output, outputAutoName, getOutput,
                         workingRepoDir, nocompress, setEnvDarcsPatches,
                        matchSeveralOrLast, depsSel,
                        ignoretimes,
                        allInteractive, umaskOption, summary, dryRun,
                        printDryRunMessageAndExit, changesReverse
                      )
import Darcs.Flags ( doReverse, UseIndex(..), ScanKnown(..), compression )
import Darcs.Match ( firstMatch, matchFirstPatchset, matchAPatchread )
import Darcs.Repository ( PatchInfoAnd, withGutsOf,
                          withRepoLock, RepoJob(..),
                    tentativelyRemovePatches, finalizeRepositoryChanges,
                    tentativelyAddToPending,
                    applyToWorking,
                    readRepo, amInRepository,
                    invalidateIndex, unrecordedChanges )
import Darcs.Patch ( RepoPatch, invert, commute, effect )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), appendPSFL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Ordered ( RL(..), (:>)(..), (+<+),
                             mapFL_FL, nullFL,
                             reverseRL, mapRL, FL(..) )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.SelectChanges ( selectChanges
                           , WhichChanges(..)
                           , selectionContext, runSelection )
import Darcs.Patch.Bundle ( makeBundleN, patchFilename, contextPatches )
import Progress ( debugMessage )
import Darcs.Witnesses.Sealed ( Sealed(..) )
import Darcs.RepoPath( useAbsoluteOrStd )
import Darcs.Lock( writeDocBinFile )
#include "gadts.h"

unrecordDescription :: String
unrecordDescription =
 "Remove recorded patches without changing the working copy."
\end{code}

Unrecord can be thought of as undo-record.
If a record is followed by an unrecord, everything looks like before
the record; all the previously unrecorded changes are back, and can be
recorded again in a new patch. The unrecorded patch however is actually
removed from your repository, so there is no way to record it again to get
it back.\footnote{The patch file itself is not actually deleted, but its
context is lost, so it cannot be reliably read---your only choice would be
to go in by hand and read its contents.}.

If you want to remove
the changes from the working copy too (where they otherwise will show
up as unrecorded changes again), you'll also need to \verb!darcs revert!.
To do unrecord and revert in one go, you can use \verb!darcs obliterate!.

If you don't revert after unrecording, then the changes made by the
unrecorded patches are left in your working tree.  If these patches are
actually from another repository, interaction (either pushes or pulls) with
that repository may be massively slowed down, as darcs tries to cope with
the fact that you appear to have made a large number of changes that
conflict with those present in the other repository.  So if you really want
to undo the result of a \emph{pull} operation, use obliterate! Unrecord is
primarily intended for when you record a patch, realize it needs just one
more change, but would rather not have a separate patch for just that one
change.

\newcommand{\pullwarning}[1]{
\textbf{WARNING:} #1 should not be run when there is a possibility
that another user may be pulling from the same repository.  Attempting to do so
may cause repository corruption.}

\pullwarning{Unrecord}

\begin{options}
--from-match, --from-patch, --from-tag, --last
\end{options}

Usually you only want to unrecord the latest changes,
and almost never would you want to unrecord changes before a tag---you
would have to have unrecorded the tag as well to do that.
Therefore, and for efficiency, darcs only prompts you for the latest patches,
after some optimal tag.

If you do want to unrecord more patches in one go,
there are the \verb!--from! and \verb!--last! options
to set the earliest patch selectable to unrecord.

\begin{options}
--matches, --patches, --tags, --no-deps
\end{options}

The \verb!--patches!, \verb!--matches!, \verb!--tags!, and \verb!--no-deps!
options can be used to select which patches to unrecord, as described in
subsection~\ref{selecting}.

With these options you can specify
what patch or patches to be prompted for by unrecord.
This is especially useful when you want to unrecord patches with dependencies,
since all the dependent patches (but no others) will be included in the choices.
Or if you use \verb!--no-deps! you won't be asked about patches that can't be
unrecorded due to depending patches.

Selecting patches can be slow, so darcs cuts the search at the last
optimized tag. Use the \verb!--from! or \verb!--last! options to search
more or fewer patches.

\begin{code}
unrecordHelp :: String
unrecordHelp =
 "Unrecord does the opposite of record in that it makes the changes from\n"++
 "patches active changes again which you may record or revert later.  The\n"++
 "working copy itself will not change.\n"++
 "Beware that you should not use this command if you are going to\n"++
 "re-record the changes in any way and there is a possibility that\n"++
 "another user may have already pulled the patch.\n"

unrecord :: DarcsCommand
unrecord = DarcsCommand {commandProgramName = "darcs",
                         commandName = "unrecord",
                         commandHelp = unrecordHelp,
                         commandDescription = unrecordDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandCommand = unrecordCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions =
                             [nocompress,umaskOption,changesReverse],
                         commandBasicOptions = [matchSeveralOrLast,
                                                 depsSel,
                                                 allInteractive,
                                                 workingRepoDir]}

unrecordCmd :: [DarcsFlag] -> [String] -> IO ()
unrecordCmd opts _ = withRepoLock opts $ RepoJob $ \repository -> do
  allpatches <- readRepo repository
  (_ :> patches) <- return $ if firstMatch opts
                             then getLastPatches opts allpatches
                             else matchingHead opts allpatches
  let context = selectionContext "unrecord" opts Nothing []
      selector = if doReverse opts
                 then selectChanges Last
                 else selectChanges LastReversed
  (_ :> to_unrecord) <- runSelection (selector patches) context
  when (nullFL to_unrecord) $ do putStrLn "No patches selected!"
                                 exitWith ExitSuccess
  putVerbose opts $ text
                      "About to write out (potentially) modified patches..."
  setEnvDarcsPatches to_unrecord
  invalidateIndex repository
-- Warning:  A do-notation statement discarded a result of type Darcs.Repository.InternalTypes.Repository p r u z.
  withGutsOf repository $ do _ <- tentativelyRemovePatches repository (compression opts) to_unrecord
                             finalizeRepositoryChanges repository
  putStrLn "Finished unrecording."

getLastPatches :: RepoPatch p => [DarcsFlag] -> PatchSet p C(Origin r)
                 -> ((PatchSet p) :> (FL (PatchInfoAnd p))) C(Origin r)
getLastPatches opts ps =
  case matchFirstPatchset opts ps of
  Sealed p1s -> findCommonWithThem ps p1s

unpullDescription :: String
unpullDescription =
 "Opposite of pull; unsafe if patch is not in remote repository."

unpullHelp :: String
unpullHelp =
 "Unpull completely removes recorded patches from your local repository.\n"++
 "The changes will be undone in your working copy and the patches will not be\n"++
 "shown in your changes list anymore.\n"++
 "Beware that if the patches are not still present in another repository you\n"++
 "will lose precious code by unpulling!\n"

unpull :: DarcsCommand
unpull = (commandAlias "unpull" Nothing obliterate)
                      {commandHelp = unpullHelp,
                       commandDescription = unpullDescription,
                       commandCommand = unpullCmd}

unpullCmd :: [DarcsFlag] -> [String] -> IO ()
unpullCmd = genericObliterateCmd "unpull"

\end{code}
\darcsCommand{obliterate}
\begin{code}

obliterateDescription :: String
obliterateDescription =
 "Delete selected patches from the repository. (UNSAFE!)"

obliterateHelp :: String
obliterateHelp =
 "Obliterate completely removes recorded patches from your local repository.\n"++
 "The changes will be undone in your working copy and the patches will not be\n"++
 "shown in your changes list anymore.\n"++
 "Beware that you can lose precious code by obliterating!\n"

\end{code}
Obliterate deletes a patch from the repository \emph{and} removes those
changes from the working directory.  It is therefore a \emph{very
dangerous} command.  When there are no local changes, obliterate is
equivalent to an unrecord followed by a revert, except that revert can be
unreverted.  In the case of tags, obliterate removes the tag itself, not
any other patches.

Note that unpull was the old name for obliterate. Unpull is still an
hidden alias for obliterate.

\pullwarning{Obliterate}

\begin{options}
--from-match, --from-patch, --from-tag, --last
\end{options}

Usually you only want to obliterate the latest changes, and almost never would
you want to obliterate changes before a tag---you would have to have obliterated
the tag as well to do that. Therefore, and for efficiency, darcs only
prompts you for the latest patches, after some optimal tag.

If you do want to obliterate more patches in one go, there are the
\verb!--from! and \verb!--last! options to set the earliest patch
selectable to obliterate.

\begin{options}
--matches, --patches, --tags, --no-deps
\end{options}

The \verb!--patches!, \verb!--matches!, \verb!--tags!, and \verb!--no-deps!
options can be used to select which patches to obliterate, as described in
subsection~\ref{selecting}.

With these options you can specify what patch or patches to be prompted for
by obliterate. This is especially useful when you want to obliterate patches with
dependencies, since all the dependent patches (but no others) will be
included in the choices. Or if you use \verb!--no-deps! you won't be asked
about patches that can't be obliterated due to depending patches.

Selecting patches can be slow, so darcs cuts the search at the last
optimized tag. Use the \verb!--from! or \verb!--last! options to search
more or fewer patches.

\begin{code}
obliterate :: DarcsCommand
obliterate = DarcsCommand {commandProgramName = "darcs",
                           commandName = "obliterate",
                           commandHelp = obliterateHelp,
                           commandDescription = obliterateDescription,
                           commandExtraArgs = 0,
                           commandExtraArgHelp = [],
                           commandCommand = obliterateCmd,
                           commandPrereq = amInRepository,
                           commandGetArgPossibilities = return [],
                           commandArgdefaults = nodefaults,
                           commandAdvancedOptions = [nocompress,ignoretimes,umaskOption, changesReverse],
                           commandBasicOptions = [matchSeveralOrLast,
                                                   depsSel,
                                                   allInteractive,
                                                   workingRepoDir,
                                                   summary,
                                                   output,
                                                   outputAutoName]++
                                                   dryRun}
obliterateCmd :: [DarcsFlag] -> [String] -> IO ()
obliterateCmd = genericObliterateCmd "obliterate"

-- | genericObliterateCmd is the function that executes the "obliterate" and
--   "unpull" commands.
genericObliterateCmd :: String      -- ^ The name under which the command is invoked (@unpull@ or @obliterate@)
                       -> [DarcsFlag] -- ^ The flags given on the command line
                       -> [String]    -- ^ Files given on the command line (unused)
                       -> IO ()
genericObliterateCmd cmdname opts _ = withRepoLock opts $ RepoJob $ \repository -> do
  -- FIXME we may need to honour --ignore-times here, although this command
  -- does not take that option (yet)
  pend <- unrecordedChanges (UseIndex, ScanKnown) repository Nothing
  allpatches <- readRepo repository
  (auto_kept :> removal_candidates) <- return $
                                        if firstMatch opts
                                        then getLastPatches opts allpatches
                                        else matchingHead opts allpatches
  let
      context = selectionContext cmdname opts Nothing []
      selector = if doReverse opts
                 then selectChanges Last
                 else selectChanges LastReversed
  (kept :> removed) <- runSelection (selector removal_candidates) context
  when (nullFL removed) $ do putStrLn "No patches selected!"
                             exitWith ExitSuccess
  case commute (effect removed :> pend) of
    Nothing -> fail $ "Can't "++ cmdname ++
               " patch without reverting some unrecorded change."
    Just (_ :> p_after_pending) -> do
        printDryRunMessageAndExit "obliterate" opts removed
        setEnvDarcsPatches removed
        when (isJust $ getOutput opts "") $
             savetoBundle opts (auto_kept `appendPSFL` kept) removed
        invalidateIndex repository
        withGutsOf repository $
-- Warning:  A do-notation statement discarded a result of type Darcs.Repository.InternalTypes.Repository p r u z.
                             do _ <- tentativelyRemovePatches repository (compression opts) removed
                                tentativelyAddToPending repository opts $ invert $ effect removed
                                finalizeRepositoryChanges repository
                                debugMessage "Applying patches to working directory..."
                                _ <- applyToWorking repository opts (invert p_after_pending) `catch` \e ->
                                    fail ("Couldn't undo patch in working dir.\n" ++ show e)
                                return ()
        putStrLn $ "Finished " ++ presentParticiple cmdname ++ "."

-- | matchingHead returns the repository up to some tag. The tag t is
-- the last tag such that there is a patch after t that is matched by
-- the user's query.
matchingHead :: forall p C(r). RepoPatch p =>
                [DarcsFlag] -> PatchSet p C(Origin r)
             -> (PatchSet p :> FL (PatchInfoAnd p)) C(Origin r)
matchingHead opts set =
    case mh set of
      (start :> patches) -> (start :> reverseRL patches)
    where
      mh :: FORALL(x) PatchSet p C(Origin x)
         -> (PatchSet p :> RL (PatchInfoAnd p)) C(Origin x)
      mh s@(PatchSet x _)
          | or (mapRL (matchAPatchread opts) x) = contextPatches s
      mh (PatchSet x (Tagged t _ ps :<: ts))
          = case mh (PatchSet (t:<:ps) ts)
            of (start :> patches) -> (start :> x +<+ patches)
      mh ps = (ps :> NilRL)

savetoBundle :: RepoPatch p => [DarcsFlag]
             -> PatchSet p C(Origin z) -> FL (PatchInfoAnd p) C(z t)
             -> IO ()
savetoBundle opts kept removed@(x :>: _) = do
    bundle <- makeBundleN Nothing kept (mapFL_FL hopefully removed)
    let filename = patchFilename $ patchDesc x
        Just outname = getOutput opts filename
    useAbsoluteOrStd writeDocBinFile putDoc outname $ bundle

savetoBundle _ _ NilFL = return ()
\end{code}

