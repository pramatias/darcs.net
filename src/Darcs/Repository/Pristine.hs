-- Copyright (C) 2002-2005 David Roundy
-- Copyright (C) 2004 Juliusz Chroboczek
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

module Darcs.Repository.Pristine ( Pristine, flagsToPristine,
                 createPristine, identifyPristine,
                 applyPristine, createPristineFromWorking,
                 easyCreatePristineDirectoryTree,
                 easyCreatePartialsPristineDirectoryTree
               ) where

import Control.Monad ( unless )
import System.Directory ( createDirectory, doesDirectoryExist, doesFileExist )

import Darcs.Flags ( DarcsFlag( PristinePlain ) )
import Darcs.Repository.Format ( RepoFormat, formatHas,
                                 RepoProperty(HashedInventory) )
import Darcs.IO ( WriteableDirectory(mWithCurrentDirectory) )
import Darcs.Patch ( Patchy, apply )
import Darcs.Patch.FileName ( fp2fn )
import Darcs.RepoPath ( FilePathLike, toFilePath )
import Darcs.External ( cloneTree, cloneTreeExcept, clonePartialsTree )
import Darcs.Repository.InternalTypes ( Pristine(..) )
import Darcs.Global ( darcsdir )
import Storage.Hashed.Darcs( writeDarcsHashed )
import Storage.Hashed.Tree( emptyTree )
#include "impossible.h"

pristineName :: String
pristineName = "pristine"

identifyPristine :: IO Pristine
identifyPristine =
    do dir <- findpristine doesDirectoryExist
       hashinv <- doesFileExist $ darcsdir++"/hashed_inventory"
       hashpris <- doesDirectoryExist hashedPristineDirectory
       case (dir, hashinv && hashpris) of
           (Nothing, False) -> return NoPristine
           (Just n, False) ->  return (PlainPristine n)
           (Nothing, True) ->  return HashedPristine
           _ -> fail "Multiple pristine trees."
    where findpristine fn =
              do e1 <- fn n1
                 e2 <- fn n2
                 case (e1, e2) of
                     (False, False) -> return Nothing
                     (True, False) -> return (Just n1)
                     (False, True) -> return (Just n2)
                     (True, True) -> fail "Multiple pristine trees."
              where  n1 = darcsdir++"/pristine"
                     n2 = darcsdir++"/current"

flagsToPristine :: [DarcsFlag] -> RepoFormat -> Pristine
flagsToPristine _ rf | formatHas HashedInventory rf = HashedPristine
flagsToPristine (PristinePlain : _) _ = PlainPristine (darcsdir++"/" ++ pristineName)
flagsToPristine (_ : t) rf = flagsToPristine t rf
flagsToPristine [] rf = flagsToPristine [PristinePlain] rf

createPristine :: Pristine -> IO ()
createPristine p =
    do oldpristine <- identifyPristine
       unless (oldpristine == NoPristine) $ fail "Pristine tree already exists."
       case p of
           NoPristine -> return ()
           PlainPristine n -> createDirectory n
           HashedPristine -> do createDirectory hashedPristineDirectory
-- Warning:  A do-notation statement discarded a result of type Storage.Hashed.Hash.Hash.
                                _ <- writeDarcsHashed emptyTree hashedPristineDirectory
                                return ()

hashedPristineDirectory :: String
hashedPristineDirectory = darcsdir++"/pristine.hashed"

applyPristine :: Patchy p => Pristine -> p C(x y) -> IO ()
-- We don't need flags for now, since we don't care about
-- SetScriptsExecutable for the pristine cache.
applyPristine (PlainPristine n) p = mWithCurrentDirectory (fp2fn n) $ apply p
applyPristine _ _
 = bug "applyPristine only works with PlainPristine."

createPristineFromWorking :: Pristine -> IO ()
createPristineFromWorking (PlainPristine n) = cloneTreeExcept [darcsdir] "." n
createPristineFromWorking _
 = bug "createPristineFromWorking only works with PlainPristine."

easyCreatePristineDirectoryTree :: Pristine -> FilePath -> IO Bool
easyCreatePristineDirectoryTree NoPristine _ = return False
easyCreatePristineDirectoryTree (PlainPristine n) p
 = cloneTree n p >> return True
easyCreatePristineDirectoryTree HashedPristine _ =
 bug "easyCreatePristineDirectoryTree does not work with HashedPristine."

-- | used in diff and dist commands
easyCreatePartialsPristineDirectoryTree :: FilePathLike fp => [fp] -> Pristine
                                        -> FilePath -> IO Bool
easyCreatePartialsPristineDirectoryTree _ NoPristine _ = return False
easyCreatePartialsPristineDirectoryTree prefs (PlainPristine n) p
 = clonePartialsTree n p (map toFilePath prefs) >> return True
easyCreatePartialsPristineDirectoryTree _ HashedPristine _ =
 bug "easyCreatePartialsPristineDirectoryTree does not work with HashedPristine."
