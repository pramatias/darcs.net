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

\darcsCommand{initialize}
\begin{code}
module Darcs.Commands.Init ( initialize, initializeCmd ) where
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag, workingRepoDir,
                        inventoryChoices )
import Darcs.Repository ( amNotInRepository, createRepository )

initializeDescription :: String
initializeDescription = "Make the current directory a repository."

initializeHelp :: String
initializeHelp =
 "The `darcs initialize' command turns the current directory into a\n" ++
 "Darcs repository.  Any existing files and subdirectories become\n" ++
 "UNSAVED changes in the working tree: record them with `darcs add -r'\n" ++
 "and `darcs record'.\n" ++
 "\n" ++
 -- FIXME: confirm my assertion re conversion is true. --twb, 2008-12-14
 "When converting a project to Darcs from some other VCS, translating\n" ++
 "the full revision history to native Darcs patches is recommended.\n" ++
 "(The Darcs wiki lists utilities for this.)  Because Darcs is optimized\n" ++
 "for small patches, simply importing the latest revision as a single\n" ++
 "large patch can PERMANENTLY degrade Darcs performance in your\n" ++
 "repository by an order of magnitude.\n" ++
 "\n" ++
 "This command creates the `_darcs' directory, which stores version\n" ++
 "control metadata.  It also contains per-repository settings in\n" ++
 "_darcs/prefs/, which you can read about in the user manual.\n" ++
 "\n" ++
 "In addition to the default `darcs-2' format, there are two backward\n" ++
 "compatibility formats for the _darcs directory.  These formats are\n" ++
 "only useful if some of your contributors do not have access to Darcs\n" ++
 "2.0 or higher.  In that case, you need to use the original format\n" ++
 "(called `old-fashioned inventory' or `darcs-1') for any repositories\n" ++
 "those contributors access.\n" ++
 "\n" ++
 "As patches cannot be shared between darcs-2 and darcs-1 repositories,\n" ++
 "you cannot use the darcs-2 format for private branches of such a\n" ++
 "project.  Instead, you should use the `hashed' format, which provides\n" ++
 "most of the features of the darcs-2 format, while retaining the\n" ++
 "ability to share patches with darcs-1 repositories.  The `darcs get'\n" ++
 "command will do this by default.\n" ++
 "\n" ++
 "Once all contributors have access to Darcs 2.0 or higher, a darcs-1\n" ++
 "project can be migrated to darcs-2 using the `darcs convert' command.\n" ++
 "\n" ++
 "Darcs will create a hashed repository by default when you `darcs get'\n" ++
 "a repository in old-fashioned inventory format.  Once all contributors\n" ++
 "have upgraded to Darcs 2.0 or later, use `darcs convert' to convert\n" ++
 "the project to the darcs-2 format.\n" ++
 "\n" ++
 "Initialize is commonly abbreviated to `init'.\n"

initialize :: DarcsCommand
initialize = DarcsCommand {commandProgramName = "darcs",
                         commandName = "initialize",
                         commandHelp = initializeHelp,
                         commandDescription = initializeDescription,
                         commandExtraArgs = 0,
                         commandExtraArgHelp = [],
                         commandPrereq = amNotInRepository,
                         commandCommand = initializeCmd,
                         commandGetArgPossibilities = return [],
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [],
                         commandBasicOptions = [inventoryChoices,
                                                  workingRepoDir]}

initializeCmd :: [DarcsFlag] -> [String] -> IO ()
initializeCmd opts _ = createRepository opts
\end{code}

