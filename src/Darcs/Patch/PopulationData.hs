--   Copyright (C) 2003-2004 Jan Scheffczyk and David Roundy
--
--   This program is free software; you can redistribute it and/or modify
--   it under the terms of the GNU General Public License as published by
--   the Free Software Foundation; either version 2, or (at your option)
--   any later version.
--
--   This program is distributed in the hope that it will be useful,
--   but WITHOUT ANY WARRANTY; without even the implied warranty of
--   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--   GNU General Public License for more details.
--
--   You should have received a copy of the GNU General Public License
--   along with this program; see the file COPYING.  If not, write to
--   the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--   Boston, MA 02110-1301, USA.

module Darcs.Patch.PopulationData ( Population(..), PopTree(..), Info(..),
                        setPopState, notModified, setState,
                        DirMark(..), getPopFrom
                      ) where

import Darcs.Utils ( withCurrentDirectory )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString       as B (ByteString)

-- | the population of a darcs repository (simpler Slurpy)
data Population pi
    = Pop { popState :: pi         -- ^ the state when last modified
          , popTree  :: PopTree pi -- ^ the directory listing
          }
 deriving (Show, Eq)

setPopState :: pi -> Population pi -> Population pi
setPopState i (Pop _ tr) = Pop i tr

-- | directory listing
data PopTree pi
    = PopDir !(Info pi) ![PopTree pi]
    | PopFile !(Info pi)
 deriving ( Ord, Eq )

-- | info of a directory member
data DirMark = AddedFile | RemovedFile | MovedFile String
             | ModifiedFile | DullFile
             | AddedDir | RemovedDir | MovedDir !String | DullDir
               deriving ( Ord, Eq )
data Info pi
    = Info {nameI :: !B.ByteString, -- ^ name of the element
            modifiedByI :: !pi, -- ^ last patch modifying this element
            modifiedHowI :: !DirMark, -- ^ how was it modified
            createdByI :: !(Maybe pi), -- ^ this can be unknown when restored backwards!
            creationNameI :: !(Maybe B.ByteString) -- ^ the original name of the element
           }
 deriving ( Ord, Eq )

-- | was an Info record not modified?
notModified :: Info pi -> Bool
notModified i = (modifiedHowI i == DullFile) || (modifiedHowI i == DullDir)

-- | set the modifier for an Info record
setState :: Info pi -> pi -> Info pi
setState i pinfo = i { modifiedByI = pinfo }

instance Show pi => Show (PopTree pi) where
 show s = showPop "" s

showPop :: Show pi => String -> PopTree pi -> String
showPop indent (PopDir i fs)
 = indent ++ show i ++ "\n" ++
   unlines (map (showPop (' ':indent)) fs)
showPop indent (PopFile i)
 = indent ++ show i

instance Show pi => Show (Info pi) where
 show i = show (nameI i) ++ " " ++ show (modifiedHowI i) ++
          " at state " ++ show (modifiedByI i)

instance Show DirMark where
 show AddedFile = "File added"
 show RemovedFile = "File removed"
 show (MovedFile s) = "File moved to " ++ s
 show ModifiedFile = "File modified"
 show DullFile = "File old"
 show AddedDir = "Dir added"
 show RemovedDir = "Dir removed"
 show (MovedDir s) = "Dir moved from " ++ s
 show DullDir = "Dir old"

-- | read the population from a given directory @dirname@
--   all folders and documents get the given time @t@
--
--  This needs to be here in order to avoid a circular dependency
--  between Population and Pristine.
getPopFrom :: forall pi . FilePath -> pi -> IO (Population pi)
getPopFrom the_directory pinfo =
    withCurrentDirectory the_directory $
       do popT <- getPopFrom_helper "."
          return (Pop pinfo popT)
 where getPopFrom_helper :: FilePath -> IO (PopTree pi)
       getPopFrom_helper dirname = do
        isdir <- doesDirectoryExist dirname
        let n = BC.pack dirname
        if isdir
          then do
           fnames <- getDirectoryContents dirname
           sl <- withCurrentDirectory dirname
                 (mapM getPopFrom_helper $ filter notHidden fnames)
           let i = Info {nameI = n,
                         modifiedByI = pinfo,
                         modifiedHowI = DullDir,
                         createdByI = Just pinfo,
                         creationNameI = Just n}
           return $ PopDir i sl
          else do let i = Info {nameI = n,
                                modifiedByI = pinfo,
                                modifiedHowI = DullFile,
                                createdByI = Just pinfo,
                                creationNameI = Just n}
                  return $ PopFile i

notHidden :: FilePath -> Bool
notHidden ('.':_) = False
notHidden ('_':_) = False
notHidden _ = True
