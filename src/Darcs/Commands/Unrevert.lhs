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

\darcsCommand{unrevert}
\begin{code}
{-# LANGUAGE CPP, GADTs #-}

#include "gadts.h"

module Darcs.Commands.Unrevert ( unrevert, writeUnrevert ) where
import System.Exit ( ExitCode(..), exitWith )
import Storage.Hashed.Tree( Tree )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Flags( diffingOpts )
import Darcs.Arguments ( DarcsFlag( MarkConflicts ),
                         ignoretimes, workingRepoDir,
                        allInteractive, umaskOption,
                      )
import Darcs.Repository ( SealedPatchSet, Repository, withRepoLock, RepoJob(..),
                          unrevertUrl, considerMergeToWorking,
                          tentativelyAddToPending, finalizeRepositoryChanges,
                          readRepo, amInRepository,
                          readRecorded,
                          applyToWorking, unrecordedChanges )
import Darcs.Patch ( RepoPatch, PrimOf, commute, namepatch, fromPrims )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (+>+) )
import Darcs.SelectChanges ( selectChanges, WhichChanges(First),
                             runSelection, selectionContextPrim )
import qualified Data.ByteString as B
import Darcs.Lock ( writeDocBinFile, removeFileMayNotExist )
import Darcs.Patch.Depends ( mergeThem )
import Darcs.Utils ( askUser, catchall )
import Darcs.Patch.Bundle ( scanBundle, makeBundleN )
import IsoDate ( getIsoDateTime )
import Darcs.SignalHandler ( withSignalsBlocked )
import Progress ( debugMessage )
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
#include "impossible.h"

unrevertDescription :: String
unrevertDescription =
 "Undo the last revert (may fail if changes after the revert)."

unrevertHelp :: String
unrevertHelp =
 "Unrevert is a rescue command in case you accidentally reverted\n" ++
 "something you wanted to keep (for example, typing `darcs rev -a'\n" ++
 "instead of `darcs rec -a').\n" ++
 "\n" ++
 "This command may fail if the repository has changed since the revert\n" ++
 "took place.  Darcs will ask for confirmation before executing an\n" ++
 "interactive command that will DEFINITELY prevent unreversion.\n"

unrevert :: DarcsCommand
unrevert = DarcsCommand {commandProgramName = "darcs",
                         commandName = "unrevert",
                         commandHelp = unrevertHelp,
                         commandDescription = unrevertDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandCommand = unrevertCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [umaskOption],
                         commandBasicOptions = [ignoretimes,
                                                  allInteractive,
                                                  workingRepoDir]}

unrevertCmd :: [DarcsFlag] -> [String] -> IO ()
unrevertCmd opts [] = withRepoLock opts $ RepoJob $ \repository -> do
  us <- readRepo repository
  Sealed them <- unrevertPatchBundle repository
  rec <- readRecorded repository
  unrec <- unrecordedChanges (diffingOpts opts {- always ScanKnown here -}) repository Nothing
  Sealed h_them <- return $ mergeThem us them
  Sealed pw <- considerMergeToWorking repository "pull" (MarkConflicts:opts) NilFL h_them
  let context = selectionContextPrim "unrevert" opts Nothing []
  (p :> skipped) <- runSelection (selectChanges First pw) context
  tentativelyAddToPending repository opts p
  withSignalsBlocked $
      do finalizeRepositoryChanges repository
         _ <- applyToWorking repository opts p `catch` \e ->
             fail ("Error applying unrevert to working directory...\n"
                   ++ show e)
         debugMessage "I'm about to writeUnrevert."
         writeUnrevert repository skipped rec (unrec+>+p)
  debugMessage "Finished unreverting."
unrevertCmd _ _ = impossible

writeUnrevert :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(x y)
               -> Tree IO -> FL (PrimOf p) C(r x) -> IO ()
writeUnrevert repository NilFL _ _ = removeFileMayNotExist $ unrevertUrl repository
writeUnrevert repository ps rec pend = do
  case commute (pend :> ps) of
    Nothing -> do really <- askUser "You will not be able to unrevert this operation! Proceed? "
                  case really of ('y':_) -> return ()
                                 _ -> exitWith $ ExitSuccess
                  writeUnrevert repository NilFL rec pend
    Just (p' :> _) -> do
        rep <- readRepo repository
        date <- getIsoDateTime
        np <- namepatch date "unrevert" "anon" [] (fromRepoPrims repository p')
        bundle <- makeBundleN (Just rec) rep (np :>: NilFL)
        writeDocBinFile (unrevertUrl repository) bundle
        where fromRepoPrims :: RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(r y) -> FL p C(r y)
              fromRepoPrims _ xs = fromPrims xs

unrevertPatchBundle :: RepoPatch p => Repository p C(r u t) -> IO (SealedPatchSet p C(Origin))
unrevertPatchBundle repository = do
  pf <- B.readFile (unrevertUrl repository)
        `catchall` fail "There's nothing to unrevert!"
  case scanBundle pf of
      Right ps -> return ps
      Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
\end{code}
