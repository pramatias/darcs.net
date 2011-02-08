%  Copyright (C) 2004,2007 David Roundy
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

\darcsCommand{amend-record}
\begin{code}
module Darcs.Commands.AmendRecord ( amendrecord ) where
import Data.Maybe ( fromMaybe, isJust )
import System.Directory ( removeFile )
import System.Exit ( ExitCode(..), exitWith )
import Control.Monad ( when, unless )

import Darcs.Flags ( DarcsFlag(Author, LogFile, PatchName, AskDeps,
                               EditLongComment, PromptLongComment, KeepDate)
                   , isInteractive
                   , diffingOpts, compression )
import Darcs.Lock ( worldReadableTemp )
import Darcs.RepoPath ( toFilePath )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully, info, patchDesc )
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..), withGutsOf,
                    tentativelyRemovePatches, tentativelyAddPatch, finalizeRepositoryChanges,
                    amInRepository
                        , invalidateIndex, unrecordedChanges
                        , testTentative
                  )
import Darcs.Patch ( RepoPatch, description, PrimOf, fromPrims,
                     infopatch, getdeps, adddeps, effect,
                   )
import Darcs.Patch.Prim ( canonizeFL )
import Darcs.Patch.Info ( piAuthor, piName, piLog, piDateString,
                          PatchInfo, patchinfo, isInverted, invertName,
                        )
import Darcs.Patch.Split ( primSplitter )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (+>+), nullFL )
import Darcs.SelectChanges ( selectChanges, WhichChanges(..),
                             selectionContextPrim,
                             runSelection,
                             withSelectedPatchFromRepo )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Commands.Record ( getDate, getLog, askAboutDepends )
import Darcs.Commands.WhatsNew ( announceFiles )
import Darcs.Arguments ( DarcsFlag ( All ),
                         fixSubPaths, setEnvDarcsFiles,
                        allInteractive, ignoretimes,
                        askLongComment, askdeps, keepDate, author, patchnameOption,
                        leaveTestDir, nocompress, lookforadds,
                         workingRepoDir,
                        matchOneNontag, umaskOption,
                         test, listRegisteredFiles,
                        getEasyAuthor, setScriptsExecutableOption
                      )
import Darcs.Utils ( askUser, clarifyErrors, PromptConfig(..), promptChar )
import Darcs.RepoPath ( SubPath() )
import Printer ( putDocLn )
#include "gadts.h"

amendrecordDescription :: String
amendrecordDescription =
 "Improve a patch before it leaves your repository."


amendrecordHelp :: String
amendrecordHelp =
 "Amend-record updates a `draft' patch with additions or improvements,\n" ++
 "resulting in a single `finished' patch.  This is better than recording\n" ++
 "the additions and improvements as separate patches, because then\n" ++
 "whenever the `draft' patch is copied between repositories, you would\n" ++
 "need to make sure all the extra patches are copied, too.\n" ++
 "\n" ++
 "Do not copy draft patches between repositories, because a finished\n" ++
 "patch cannot be copied into a repository that contains a draft of the\n" ++
 "same patch.  If this has already happened, `darcs obliterate' can be\n" ++
 "used to remove the draft patch.\n" ++
 "\n" ++
 -- FIXME: is the following still true in Darcs 2.1? --twb
 "Do not run amend-record in repository that other developers can pull\n" ++
 "from, because if they pull while an amend-record is in progress, their\n" ++
 "repository may be corrupted.\n" ++
 "\n" ++
 "When recording a draft patch, it is a good idea to start the name with\n" ++
 "`DRAFT:' so that other developers know it is not finished.  When\n" ++
 "finished, remove it with `darcs amend-record --edit-long-comment'.\n" ++
 "To change the patch name without starting an editor, use --patch-name.\n" ++
 "\n" ++
 "Like `darcs record', if you call amend-record with files as arguments,\n" ++
 "you will only be asked about changes to those files.  So to amend a\n" ++
 "patch to foo.c with improvements in bar.c, you would run:\n" ++
 "\n" ++
 "    darcs amend-record --match 'touch foo.c' bar.c\n" ++
 "\n" ++
 "It is usually a bad idea to amend another developer's patch.  To make\n" ++
 "amend-record only ask about your own patches by default, you can add\n" ++
 "something like `amend-record match David Roundy' to ~/.darcs/defaults, \n" ++
 "where `David Roundy' is your name. " ++
 "On Windows use C:/Documents And Settings/user/Application Data/darcs/defaults\n"

amendrecord :: DarcsCommand
amendrecord = DarcsCommand {commandProgramName = "darcs",
                            commandName = "amend-record",
                            commandHelp = amendrecordHelp,
                            commandDescription = amendrecordDescription,
                            commandExtraArgs = -1,
                            commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                            commandCommand = amendrecordCmd,
                            commandPrereq = amInRepository,
                            commandGetArgPossibilities = listRegisteredFiles,
                            commandArgdefaults = nodefaults,
                            commandAdvancedOptions = [nocompress, ignoretimes, umaskOption,
                                                        setScriptsExecutableOption],
                            commandBasicOptions = [matchOneNontag,
                                                   test,
                                                    leaveTestDir,
                                                    allInteractive,
                                                    author, patchnameOption, askdeps, askLongComment, keepDate,
                                                    lookforadds,
                                                    workingRepoDir]}

amendrecordCmd :: [DarcsFlag] -> [String] -> IO ()
amendrecordCmd opts args = if null args
  then doAmendRecord opts Nothing
  else do
    files <- fixSubPaths opts args
    if null files
      then fail "No valid arguments were given, nothing to do."
      else doAmendRecord opts $ Just files

doAmendRecord :: [DarcsFlag] -> Maybe [SubPath] -> IO ()
doAmendRecord opts files =
    withRepoLock opts $ RepoJob $ \(repository :: Repository p C(r u r)) -> do
    withSelectedPatchFromRepo "amend" repository opts $ \ (_ :> oldp) -> do
        announceFiles files "Amending changes in"
        ch <- unrecordedChanges (diffingOpts opts) repository files
        case ch of
          NilFL | not (hasEditMetadata opts) -> putStrLn "No changes!"
          _ -> do
            let context = selectionContextPrim  "add" (filter (==All) opts) (Just primSplitter)
                                                                            (map toFilePath $ fromMaybe [] files)
            chosenPatches <- runSelection (selectChanges First ch) context
            addChangesToPatch opts repository oldp chosenPatches

addChangesToPatch :: forall p C(r u t x y) . (RepoPatch p)
                  => [DarcsFlag] -> Repository p C(r u t) -> PatchInfoAnd p C(x t)
                  -> (FL (PrimOf p) :> FL (PrimOf p)) C(t y) -> IO ()
addChangesToPatch opts repository oldp (chs:>_) =
                  if (nullFL chs && not (hasEditMetadata opts))
                  then putStrLn "You don't want to record anything!"
                  else do
                       invalidateIndex repository
                       withGutsOf repository $ do
                         repository' <- tentativelyRemovePatches repository (compression opts)
                                                                 (oldp :>: NilFL)
                         (mlogf, newp) <- updatePatchHeader opts repository' oldp chs
                         setEnvDarcsFiles newp
                         repository'' <- tentativelyAddPatch repository' (compression opts) newp
                         let failmsg = maybe "" (\lf -> "\nLogfile left in "++lf++".") mlogf
                         rc <- testTentative repository
                         when (rc /= ExitSuccess) $ do
                             when (not $ isInteractive opts) $ exitWith rc `clarifyErrors` failmsg
                             putStrLn $ "Looks like you have a bad patch: '" ++ patchDesc newp ++ "'"
                             let prompt = "Shall I amend it anyway?"
                             yn <- promptChar (PromptConfig prompt "yn" [] (Just 'n') [])
                             case yn of
                               'y' -> return ()
                               _ -> exitWith rc `clarifyErrors` failmsg
                         finalizeRepositoryChanges repository'' `clarifyErrors` failmsg
                         maybe (return ()) removeFile mlogf
                         putStrLn "Finished amending patch:"
                         putDocLn $ description newp

updatePatchHeader :: forall p C(x y r u t) . (RepoPatch p)
                  => [DarcsFlag] -> Repository p C(r u t)
                  -> PatchInfoAnd p C(t x) -> FL (PrimOf p) C(x y)
                  -> IO (Maybe String, PatchInfoAnd p C(t y))
updatePatchHeader opts repository oldp chs = do

                       let newchs = canonizeFL (effect oldp +>+ chs)

                       let old_pdeps = getdeps $ hopefully oldp
                       newdeps <- if AskDeps `elem` opts
                                  then askAboutDepends repository newchs opts old_pdeps
                                  else return old_pdeps

                       let old_pinf = info oldp
                           prior    = (piName old_pinf, piLog old_pinf)
                           make_log = worldReadableTemp "darcs-amend-record"
                           old_author = piAuthor old_pinf
                       date <- if KeepDate `elem` opts then return (piDateString old_pinf) else getDate opts
                       warnIfHijacking opts old_author
                       (new_name, new_log, mlogf) <- getLog opts (Just prior) make_log chs
                       let new_author = case getAuthor opts of
                                        Just a  -> a
                                        Nothing -> piAuthor old_pinf
                           maybe_invert = if isInverted old_pinf then invertName else id
                       new_pinf <- maybe_invert `fmap` patchinfo date new_name
                                                                 new_author new_log

                       let newp = n2pia (adddeps (infopatch new_pinf (fromPrims newchs)) newdeps)

                       return (mlogf, newp)

warnIfHijacking :: [DarcsFlag] -> String -> IO ()
warnIfHijacking opts old_author = do
  authors_here <- getEasyAuthor
  let edit_author = isJust (getAuthor opts)
  unless (edit_author || any (== old_author) authors_here) $
    do yorn <- askUser $
        "You're not "++old_author ++"! Amend anyway? "
       case yorn of ('y':_) -> return ()
                    _       -> exitWith ExitSuccess

hasEditMetadata :: [DarcsFlag] -> Bool
hasEditMetadata (Author _:_) = True
hasEditMetadata (LogFile _:_) = True
hasEditMetadata (PatchName _:_) = True
hasEditMetadata (EditLongComment:_) = True
hasEditMetadata (PromptLongComment:_) = True
hasEditMetadata (AskDeps:_) = True
hasEditMetadata (_:fs) = hasEditMetadata fs
hasEditMetadata [] = False

getAuthor :: [DarcsFlag] -> Maybe String
getAuthor (Author a:_) = Just a
getAuthor (_:as) = getAuthor as
getAuthor []     = Nothing

\end{code}
