-- Copyright (C) 2006 Tommy Pettersson <ptp@lysator.liu.se>
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

#include "gadts.h"

module Darcs.CommandsAux ( checkPaths, maliciousPatches, hasMaliciousPath,
                        ) where
import Darcs.Flags ( DarcsFlag( RestrictPaths, DontRestrictPaths ) )
import Darcs.Patch ( Patchy, listTouchedFiles )
import Darcs.Witnesses.Ordered ( FL, mapFL )
import Darcs.Witnesses.Sealed ( Sealed2(..), unseal2 )
import Darcs.Global ( darcsdir )
import Data.List ( intersect )
import System.FilePath ( splitDirectories )

-- * File paths
{-
  Darcs will operate on files and directories with the invoking user's
  privileges. The paths for these files and directories are stored in
  patches, which darcs receives in various ways. Even though darcs will not
  create patches with "unexpected" file paths, there are no such guarantees
  for received patches. A spoofed patch could inflict changes on any file
  or directory which the invoking user is privileged to modify.

  There is no one single "apply" function that can check paths, so each
  command is responsible for not applying patches without first checking
  them with one of these function when appropriate.
-}

{- |
  A convenience function to call from all darcs command functions before
  applying any patches. It checks for malicious paths in patches, and
  prints an error message and fails if it finds one.
-}
checkPaths :: Patchy p => [DarcsFlag] -> FL p C(x y) -> IO ()
checkPaths opts patches
  = if check_is_on  && or (mapFL hasMaliciousPath patches)
      then fail $ unlines $ ["Malicious path in patch:"] ++
                            (map (\s -> "    " ++ s) $ concat $ mapFL maliciousPaths patches) ++
                            ["", "If you are sure this is ok then you can run again with the --dont-restrict-paths option."]
           -- TODO: print patch(es)
           -- NOTE: should use safe Doc printer, this can be evil chars
      else return ()
 where
    check_is_on = DontRestrictPaths `notElem` opts  ||
                  RestrictPaths        `elem` opts

-- | Filter out patches that contains some malicious file path
maliciousPatches :: Patchy p => [Sealed2 p] -> [Sealed2 p]
maliciousPatches to_check = filter (unseal2 hasMaliciousPath) to_check

hasMaliciousPath :: Patchy p => p C(x y) -> Bool
hasMaliciousPath patch =
    case maliciousPaths patch of
      [] -> False
      _ -> True

maliciousPaths :: Patchy p => p C(x y) -> [String]
maliciousPaths patch =
  let paths = listTouchedFiles patch in
    filter isMaliciousPath paths

{-|
  What is a malicious path?

  A spoofed path is a malicious path.

  1. Darcs only creates explicitly relative paths (beginning with @\".\/\"@),
     so any not explicitly relative path is surely spoofed.

  2. Darcs normalizes paths so they never contain @\"\/..\/\"@, so paths with
     @\"\/..\/\"@ are surely spoofed.

  A path to a darcs repository's meta data can modify \"trusted\" patches or
  change safety defaults in that repository, so we check for paths
  containing @\"\/_darcs\/\"@ which is the entry to darcs meta data.

  To do?

  * How about get repositories?

  * Would it be worth adding a --semi-safe-paths option for allowing
    changes to certain preference files (_darcs\/prefs\/) in sub
    repositories'?
-}
isMaliciousPath :: String -> Bool
isMaliciousPath fp =
    not (isExplicitlyRelative fp) ||
    splitDirectories fp `contains_any` [ "..", darcsdir ]
 where
    contains_any a b = not . null $ intersect a b

isExplicitlyRelative :: String -> Bool
isExplicitlyRelative ('.':'/':_) = True  -- begins with "./"
isExplicitlyRelative _ = False
