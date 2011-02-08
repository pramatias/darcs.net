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

{-# LANGUAGE CPP, EmptyDataDecls #-}

#include "gadts.h"

module Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, Origin,
                         progressPatchSet, tags, appendPSFL,
                         newset2RL, newset2FL ) where

import Progress ( progress )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info )
import Darcs.Witnesses.Ordered ( FL, RL(..), (+<+), reverseFL,
                                 reverseRL, mapRL_RL, concatRL, mapRL )
import Darcs.Witnesses.Sealed ( Sealed )

data Origin

type SealedPatchSet p C(start) = Sealed ((PatchSet p) C(start))

data PatchSet p C(start x) where
    PatchSet :: RL (PatchInfoAnd p) C(y x) -> RL (Tagged p) C(start y) -> PatchSet p C(start x)

data Tagged p C(x2 z) where
    Tagged :: PatchInfoAnd p C(y z) -> Maybe String
           -> RL (PatchInfoAnd p) C(x3 y) -> Tagged p C(x3 z)

newset2RL :: PatchSet p C(start x4) -> RL (PatchInfoAnd p) C(start x4)
newset2RL (PatchSet ps ts) = ps +<+ concatRL (mapRL_RL ts2rl ts)
    where ts2rl :: Tagged p C(x5 y) -> RL (PatchInfoAnd p) C(x5 y)
          ts2rl (Tagged t _ ps2) = t :<: ps2

newset2FL :: PatchSet p C(start x6) -> FL (PatchInfoAnd p) C(start x6)
newset2FL = reverseRL . newset2RL

appendPSFL :: PatchSet p C(start x) -> FL (PatchInfoAnd p) C(x y)
           -> PatchSet p C(start y)
appendPSFL (PatchSet ps ts) newps = PatchSet (reverseFL newps +<+ ps) ts

progressPatchSet :: String -> PatchSet p C(start x7) -> PatchSet p C(start x7)
progressPatchSet k (PatchSet ps0 ts0) = PatchSet (mapRL_RL prog ps0) $ mapRL_RL pts ts0
    where prog = progress k
          pts :: Tagged p C(x8 y) -> Tagged p C(x8 y)
          pts (Tagged t h ps) = Tagged (prog t) h (mapRL_RL prog ps)

tags :: PatchSet p C(start x13) -> [PatchInfo]
tags (PatchSet _ ts) = mapRL f ts
    where f :: Tagged p C(x14 y) -> PatchInfo
          f (Tagged t _ _) = info t
