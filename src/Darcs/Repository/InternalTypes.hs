-- Copyright (C) 2006-2007 David Roundy
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
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Repository.InternalTypes ( Repository(..), RepoType(..), Pristine(..)
                                      , extractCache, extractOptions, modifyCache
                                      ) where

import Data.List ( nub, sortBy )
import Darcs.Repository.Cache ( Cache (..) , compareByLocality )
import Darcs.Flags ( DarcsFlag )
import Darcs.Repository.Format ( RepoFormat )
import Darcs.Patch ( RepoPatch )

data Pristine
  = NoPristine
  | PlainPristine !String
  | HashedPristine
    deriving ( Show, Eq )

data Repository (p :: * C(-> * -> *)) C(recordedstate unrecordedstate tentativestate) = Repo !String ![DarcsFlag] !RepoFormat !(RepoType p) deriving ( Show )

data RepoType (p :: * C(-> * -> *)) = DarcsRepository !Pristine Cache deriving ( Show )

extractCache :: Repository p C(r u t) -> Cache
extractCache (Repo _ _ _ (DarcsRepository _ c)) = c

extractOptions :: Repository p C(r u t) -> [DarcsFlag]
extractOptions (Repo _ opts _ _) = opts

-- | 'modifyCache' @repository function@ modifies the cache of
--   @repository@ with @function@, remove duplicates and sort the results with 'compareByLocality'.
modifyCache :: FORALL(p r u t) (RepoPatch p)  => Repository p C(r u t) -> (Cache -> Cache) -> Repository p C(r u t)
modifyCache (Repo dir opts rf (DarcsRepository pristine cache)) f = Repo dir opts rf dr
  where dr            = DarcsRepository pristine . cmap ( sortBy compareByLocality . nub ) $ f cache
        cmap g (Ca c) = Ca (g c)
