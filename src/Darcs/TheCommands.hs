-- Copyright (C) 2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP #-}
module Darcs.TheCommands ( commandControlList ) where

import Prelude ()
import Darcs.Commands.Add ( add )
import Darcs.Commands.AmendRecord ( amendrecord )
import Darcs.Commands.Annotate ( annotate )
import Darcs.Commands.Apply ( apply )
import Darcs.Commands.Changes ( changes, log )
import Darcs.Commands.Check ( check )
import Darcs.Commands.Convert ( convert )
import Darcs.Commands.Diff ( diffCommand )
import Darcs.Commands.Dist ( dist )
import Darcs.Commands.Get ( get, clone )
import Darcs.Commands.GZCRCs ( gzcrcs )
import Darcs.Commands.Init ( initialize )
import Darcs.Commands.Show ( showCommand, list, query )
import Darcs.Commands.MarkConflicts ( markconflicts, resolve )
import Darcs.Commands.Move ( move, mv )
import Darcs.Commands.Optimize ( optimize )
import Darcs.Commands.Pull ( pull, fetch )
import Darcs.Commands.Push ( push )
import Darcs.Commands.Put ( put )
import Darcs.Commands.Record ( record, commit )
import Darcs.Commands.Remove ( remove, rm, unadd )
import Darcs.Commands.Repair ( repair )
import Darcs.Commands.Replace ( replace )
import Darcs.Commands.Revert ( revert )
import Darcs.Commands.Rollback ( rollback )
import Darcs.Commands.Send ( send )
import Darcs.Commands.SetPref ( setpref )
import Darcs.Commands.Tag ( tag )
import Darcs.Commands.TrackDown ( trackdown )
import Darcs.Commands.TransferMode ( transferMode )
import Darcs.Commands.Unrecord ( unrecord, unpull, obliterate )
import Darcs.Commands.Unrevert ( unrevert )
import Darcs.Commands.WhatsNew ( whatsnew, status )
import Darcs.Commands ( CommandControl(CommandData,HiddenCommand,GroupName) )

-- | The commands that darcs knows about (e.g. whatsnew, record),
--   organized into thematic groups.  Note that hidden commands
--   are also listed here.
commandControlList :: [CommandControl]
commandControlList = [GroupName "Changing and querying the working copy:",
                CommandData add,
                CommandData remove, HiddenCommand unadd, HiddenCommand rm,
                CommandData move, HiddenCommand mv,
                CommandData replace,
                CommandData revert,
                CommandData unrevert,
                CommandData whatsnew, HiddenCommand status,
                GroupName "Copying changes between the working copy and the repository:",
                CommandData record, HiddenCommand commit,
                CommandData unrecord,
                CommandData amendrecord,
                CommandData markconflicts, HiddenCommand resolve,
                GroupName "Direct modification of the repository:",
                CommandData tag,
                CommandData setpref,
                GroupName "Querying the repository:",
                CommandData diffCommand,
                CommandData changes, HiddenCommand log,
                CommandData annotate,
                CommandData dist,
                CommandData trackdown,
                CommandData showCommand, HiddenCommand list, HiddenCommand query,
                HiddenCommand transferMode,
                GroupName "Copying patches between repositories with working copy update:",
                CommandData pull,
                CommandData fetch,
                CommandData obliterate, HiddenCommand unpull,
                CommandData rollback,
                CommandData push,
                CommandData send,
                CommandData apply,
                CommandData get, HiddenCommand clone,
                CommandData put,
                GroupName "Administrating repositories:",
                CommandData initialize,
                CommandData optimize,
                CommandData check,
                CommandData repair,
                CommandData convert
                ,HiddenCommand gzcrcs
               ]
