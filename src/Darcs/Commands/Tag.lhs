%  Copyright (C) 2003-2004 David Roundy
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

\darcsCommand{tag}
\begin{code}
module Darcs.Commands.Tag ( tag ) where
import System.Directory ( removeFile )
import Control.Monad ( when )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( nocompress, umaskOption, patchnameOption, author,
                         pipeInteractive, askLongComment,
                         workingRepoDir, getAuthor )
import Darcs.Patch.PatchInfoAnd ( n2pia )
import Darcs.Repository ( amInRepository, withRepoLock, Repository, RepoJob(..), readRepo,
                    tentativelyAddPatch, finalizeRepositoryChanges,
                  )
import Darcs.Patch ( infopatch, adddeps, Patchy, PrimPatch, PrimOf )
import Darcs.Patch.Info ( patchinfo )
import Darcs.Patch.Depends ( getTagsRight )
import Darcs.Commands.Record ( getDate, getLog )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Lock ( worldReadableTemp )
import Darcs.Flags ( DarcsFlag(..), compression )
import System.IO ( hPutStr, stderr )

#include "gadts.h"

tagDescription :: String
tagDescription = "Name the current repository state for future reference."

tagHelp :: String
tagHelp =
 "The `darcs tag' command names the current repository state, so that it\n" ++
 "can easily be referred to later.  Every `important' state should be\n" ++
 "tagged; in particular it is good practice to tag each stable release\n" ++
 "with a number or codename.  Advice on release numbering can be found\n" ++
 "at http://producingoss.com/en/development-cycle.html.\n" ++
 "\n" ++
 "To reproduce the state of a repository `R' as at tag `t', use the\n" ++
 "command `darcs get --tag t R'.  The command `darcs show tags' lists\n" ++
 "all tags in the current repository.\n" ++
 "\n" ++
 "Tagging also provides significant performance benefits: when Darcs\n" ++
 "reaches a shared tag that depends on all antecedent patches, it can\n" ++
 "simply stop processing.\n" ++
 "\n" ++
 "Like normal patches, a tag has a name, an author, a timestamp and an\n" ++
 "optional long description, but it does not change the working tree.\n" ++
 "A tag can have any name, but it is generally best to pick a naming\n" ++
 "scheme and stick to it.\n" ++
 "\n" ++
 "The `darcs tag' command accepts the --pipe option, which behaves as\n" ++
 "described in `darcs record'.\n"

tag :: DarcsCommand
tag = DarcsCommand {commandProgramName = "darcs",
                    commandName = "tag",
                    commandHelp = tagHelp,
                    commandDescription = tagDescription,
                    commandExtraArgs = -1,
                    commandExtraArgHelp = ["[TAGNAME]"],
                    commandCommand = tagCmd,
                    commandPrereq = amInRepository,
                    commandGetArgPossibilities = return [],
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = [nocompress,umaskOption],
                    commandBasicOptions = [patchnameOption, author,
                                            pipeInteractive,
                                            askLongComment,
                                            workingRepoDir]}

tagCmd :: [DarcsFlag] -> [String] -> IO ()
tagCmd opts args = withRepoLock opts $ RepoJob $ \(repository :: Repository p C(r u r)) -> do
    date <- getDate opts
    the_author <- getAuthor opts
    deps <- getTagsRight `fmap` readRepo repository
    (name, long_comment, mlogf)  <- get_name_log (NilFL :: FL (PrimOf p) C(a a)) opts args
    myinfo <- patchinfo date name the_author long_comment
    let mypatch = infopatch myinfo NilFL
-- Warning:  A do-notation statement discarded a result of type Darcs.Repository.InternalTypes.Repository p r u r.
    _ <- tentativelyAddPatch repository (compression opts) $ n2pia $ adddeps mypatch deps
    finalizeRepositoryChanges repository
    maybe (return ()) removeFile mlogf
    putStrLn $ "Finished tagging patch '"++name++"'"
  where  get_name_log ::(Patchy prim, PrimPatch prim) => FL prim C(a a) -> [DarcsFlag] -> [String] -> IO (String, [String], Maybe String)
         get_name_log nilFL o a
                          = do let o2 = if null a then o else (add_patch_name o (unwords a))
                               (name, comment, mlogf) <- getLog o2 Nothing (worldReadableTemp "darcs-tag") nilFL
                               when (length name < 2) $ hPutStr stderr $
                                 "Do you really want to tag '"
                                 ++name++"'? If not type: darcs obliterate --last=1\n"
                               return ("TAG " ++ name, comment, mlogf)
         add_patch_name :: [DarcsFlag] -> String -> [DarcsFlag]
         add_patch_name o a| has_patch_name o = o
                           | otherwise = [PatchName a] ++ o
         has_patch_name (PatchName _:_) = True
         has_patch_name (_:fs) = has_patch_name fs
         has_patch_name [] = False

-- This may be useful for developers, but users don't care about
-- internals:
--
-- A tagged version automatically depends on all patches in the
-- repository.  This allows you to later reproduce precisely that
-- version.  The tag does this by depending on all patches in the
-- repository, except for those which are depended upon by other tags
-- already in the repository.  In the common case of a sequential
-- series of tags, this means that the tag depends on all patches
-- since the last tag, plus that tag itself.
\end{code}
