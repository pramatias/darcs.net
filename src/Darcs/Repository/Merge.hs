-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
-- Copyright (C) 2009 Petr Rockai
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

#include "gadts.h"

module Darcs.Repository.Merge ( tentativelyMergePatches, considerMergeToWorking ) where

import Darcs.Resolution ( standardResolution, externalResolution )
import Darcs.External ( backupByCopying )
import Control.Monad ( when, unless )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, hopefully )
import Darcs.Flags
    ( DarcsFlag( AllowConflicts, NoAllowConflicts ), wantExternalMerge, diffingOpts, compression )
import Darcs.Witnesses.Ordered
    ( FL(..), (:\/:)(..), (:/\:)(..), (+>+), mapFL_FL )
import Darcs.Patch
    ( RepoPatch, PrimOf, merge, joinPatches, listTouchedFiles
    , patchcontents, anonymous, fromPrims, effect )
import Darcs.Patch.Depends( merge2FL )
import Progress( debugMessage )
import Darcs.ProgressPatches( progressFL )
import Darcs.Witnesses.Sealed( Sealed(Sealed), seal )
import Darcs.Repository.InternalTypes( Repository(..) )

import Darcs.Repository.State( unrecordedChanges, readUnrecorded )

import Darcs.Repository.Internal
    ( announceMergeConflicts, checkUnrecordedConflicts
    , MakeChanges(..), setTentativePending
    , tentativelyAddPatch_, applyToTentativePristine, UpdatePristine(..) )

tentativelyMergePatches_ :: forall p C(r u t y x). RepoPatch p
                         => MakeChanges
                         -> Repository p C(r u t) -> String -> [DarcsFlag]
                         -> FL (PatchInfoAnd p) C(x t) -> FL (PatchInfoAnd p) C(x y)
                         -> IO (Sealed (FL (PrimOf p) C(u)))
tentativelyMergePatches_ mc r cmd opts usi themi =
  do let us = mapFL_FL hopefully usi
         them = mapFL_FL hopefully themi
     Sealed pc <- return $ merge2FL (progressFL "Merging us" usi) (progressFL "Merging them" themi)
     pend <- unrecordedChanges (diffingOpts opts) r Nothing
     anonpend <- n2pia `fmap` anonymous (fromPrims pend)
     pend' :/\: pw <- return $ merge (pc :\/: anonpend :>: NilFL)
     let pwprim = joinPatches $ progressFL "Examining patches for conflicts" $
                                mapFL_FL (patchcontents . hopefully) pw
     Sealed standard_resolved_pw <- return $ standardResolution pwprim
     debugMessage "Checking for conflicts..."
     unless (AllowConflicts `elem` opts || NoAllowConflicts `elem` opts) $
            mapM_ backupByCopying $ listTouchedFiles standard_resolved_pw
     debugMessage "Announcing conflicts..."
     have_conflicts <- announceMergeConflicts cmd opts standard_resolved_pw
     debugMessage "Checking for unrecorded conflicts..."
     have_unrecorded_conflicts <- checkUnrecordedConflicts opts $ mapFL_FL hopefully pc
     debugMessage "Reading working directory..."
     working <- readUnrecorded r Nothing
     debugMessage "Working out conflicts in actual working directory..."
     Sealed pw_resolution <-
          case (wantExternalMerge opts, have_conflicts || have_unrecorded_conflicts) of
          (Nothing,_) -> return $ if AllowConflicts `elem` opts
                                  then seal NilFL
                                  else seal standard_resolved_pw
          (_,False) -> return $ seal standard_resolved_pw
          (Just c, True) -> externalResolution working c
                                                    (effect us +>+ pend)
                                                    (effect them) pwprim
     debugMessage "Applying patches to the local directories..."
     when (mc == MakeChanges) $
          do let doChanges :: FL (PatchInfoAnd p) C(x t) -> IO ()
                 doChanges NilFL = applyps r themi
                 doChanges _     = applyps r pc
             doChanges usi
             setTentativePending r (effect pend' +>+ pw_resolution)
     return $ seal (effect pwprim +>+ pw_resolution)
  where mapAdd :: Repository p C(m l i) -> FL (PatchInfoAnd p) C(i j) -> IO (Repository p C(m l j))
        mapAdd repo NilFL = return repo
        mapAdd repo (a:>:as) =
               do repo' <- tentativelyAddPatch_ DontUpdatePristine repo (compression opts) a
                  mapAdd repo' as
        applyps :: Repository p C(m l i) -> FL (PatchInfoAnd p) C(i j) -> IO ()
        applyps repo ps = do debugMessage "Adding patches to inventory..."
-- Warning:  A do-notation statement discarded a result of type Repository p m l j.
                             _ <- mapAdd repo ps
                             debugMessage "Applying patches to pristine..."
                             applyToTentativePristine repo ps

tentativelyMergePatches :: RepoPatch p
                        => Repository p C(r u t) -> String -> [DarcsFlag]
                        -> FL (PatchInfoAnd p) C(x t) -> FL (PatchInfoAnd p) C(x y)
                        -> IO (Sealed (FL (PrimOf p) C(u)))
tentativelyMergePatches = tentativelyMergePatches_ MakeChanges


considerMergeToWorking :: RepoPatch p
                       => Repository p C(r u t) -> String -> [DarcsFlag]
                       -> FL (PatchInfoAnd p) C(x t) -> FL (PatchInfoAnd p) C(x y)
                       -> IO (Sealed (FL (PrimOf p) C(u)))
considerMergeToWorking = tentativelyMergePatches_ DontMakeChanges

