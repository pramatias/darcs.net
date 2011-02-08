%  Copyright (C) 2007 Kevin Quick
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

\darcsCommand{show repo}

\begin{code}
{-# LANGUAGE CPP #-}
#include "gadts.h"
module Darcs.Commands.ShowRepo ( showRepo ) where

import Data.Char ( toLower, isSpace )
import Data.List ( intersperse )
import Control.Monad ( when, unless )
import Text.Html ( tag, stringToHtml )
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir, files, xmloutput )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Repository ( withRepository, RepoJob(..), amInRepository, readRepo )
import Darcs.Repository.Internal ( Repository(..), RepoType(..) )
import Darcs.Repository.Format ( RepoFormat(..) )
import Darcs.Repository.Prefs ( getPreflist )
import Darcs.Repository.Motd ( getMotd )
import Darcs.Patch ( RepoPatch )
import Darcs.Patch.Set ( newset2RL )
import Darcs.Witnesses.Ordered ( lengthRL )
import qualified Data.ByteString.Char8 as BC  (unpack)

showRepoHelp :: String
showRepoHelp =
 "The `darcs show repo' command displays statistics about the current\n" ++
 "repository, allowing third-party scripts to access this information\n" ++
 "without inspecting _darcs directly (and without breaking when the\n" ++
 "_darcs format changes).\n" ++
 "\n" ++
 "By default, the number of patches is shown.  If this data isn't\n" ++
 "needed, use --no-files to accelerate this command from O(n) to O(1).\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The --xml-output\n" ++
 "option can be used to generate output for machine postprocessing.\n"

showRepoDescription :: String
showRepoDescription = "Show repository summary information"

showRepo :: DarcsCommand
showRepo = DarcsCommand { commandProgramName = "darcs",
                           commandName = "repo",
                           commandHelp = showRepoHelp,
                           commandDescription = showRepoDescription,
                           commandExtraArgs = 0,
                           commandExtraArgHelp = [],
                           commandCommand = repoCmd,
                           commandPrereq = amInRepository,
                           commandGetArgPossibilities = return [],
                           commandArgdefaults = nodefaults,
                           commandAdvancedOptions = [],
                           commandBasicOptions = [workingRepoDir, files, xmloutput] }

repoCmd :: [DarcsFlag] -> [String] -> IO ()
repoCmd opts _ = let put_mode = if XMLOutput `elem` opts then showInfoXML else showInfoUsr
                  in withRepository opts $ RepoJob $ \repository -> actuallyShowRepo (putInfo put_mode) repository

-- Some convenience functions to output a labelled text string or an
-- XML tag + value (same API).  If no value, output is suppressed
-- entirely.  Borrow some help from Text.Html to perform XML output.

type ShowInfo = String -> String -> String

showInfoXML :: ShowInfo
showInfoXML t i = show $ tag (safeTag t) $ stringToHtml i

safeTag :: String -> String
safeTag [] = []
safeTag (' ':cs) = safeTag cs
safeTag ('#':cs) = "num_" ++ (safeTag cs)
safeTag (c:cs) = toLower c : safeTag cs

-- labelled strings: labels are right-aligned at 14 characters;
-- subsequent lines in multi-line output are indented accordingly.
showInfoUsr :: ShowInfo
showInfoUsr t i = (replicate (14 - length(t)) ' ') ++ t ++ ": " ++
                  (concat $ intersperse ('\n' : (replicate 16 ' ')) $ lines i) ++ "\n"

type PutInfo = String -> String -> IO ()
putInfo :: ShowInfo -> PutInfo
putInfo m t i = unless (null i) (putStr $ m t i)

-- Primary show-repo operation.  Determines ordering of output for
-- sub-displays.  The `out' argument is one of the above operations to
-- output a labelled text string or an XML tag and contained value.

actuallyShowRepo :: RepoPatch p => PutInfo -> Repository p C(r u r) -> IO ()
actuallyShowRepo out r@(Repo loc opts rf rt) = do
         when (XMLOutput `elem` opts) (putStr "<repository>\n")
         showRepoType out rt
         when (Verbose `elem` opts) (out "Show" $ show r)
         showRepoFormat out rf
         out "Root" loc
         showRepoAux out rt
         showRepoPrefs out
         unless (NoFiles `elem` opts) (numPatches r >>= (out "Num Patches" . show ))
         showRepoMOTD out r
         when (XMLOutput `elem` opts) (putStr "</repository>\n")

-- Most of the actual elements being displayed are part of the Show
-- class; that's fine for a Haskeller, but not for the common user, so
-- the routines below work to provide more human-readable information
-- regarding the repository elements.

showRepoType :: PutInfo -> RepoType p -> IO ()
showRepoType out (DarcsRepository _ _) = out "Type" "darcs"

showRepoFormat :: PutInfo -> RepoFormat -> IO ()
showRepoFormat out (RF rf) = out "Format" $
    concat $ intersperse ", " (map (concat . intersperse "|" . map BC.unpack) rf)

showRepoAux :: PutInfo -> RepoType p -> IO ()
showRepoAux out (DarcsRepository pris cs) =
    do out "Pristine" $ show pris
       out "Cache" $ concat $ intersperse ", " $ lines $ show cs


showRepoPrefs :: PutInfo -> IO ()
showRepoPrefs out = do
    getPreflist "prefs" >>= mapM_ prefOut
    getPreflist "author" >>= out "Author" . unlines
    getPreflist "defaultrepo" >>= out "Default Remote" . unlines
  where prefOut = uncurry out . (\(p,v) -> (p++" Pref", (dropWhile isSpace v))) . break isSpace

showRepoMOTD :: RepoPatch p => PutInfo -> Repository p C(r u r) -> IO ()
showRepoMOTD out (Repo loc _ _ _) = getMotd loc >>= out "MOTD" . BC.unpack

-- Support routines to provide information used by the PutInfo operations above.

numPatches :: RepoPatch p => Repository p C(r u r) -> IO Int
numPatches r = readRepo r >>= (return . lengthRL . newset2RL)

\end{code}
