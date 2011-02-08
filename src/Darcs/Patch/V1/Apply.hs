{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Apply () where

import Darcs.Patch.Apply ( Apply, apply )
import Darcs.Patch.Prim ( PrimPatch, applyPrimFL )
import Darcs.Patch.Repair ( RepairToFL, applyAndTryToFixFL,
                            mapMaybeSnd )
import Darcs.Patch.Effect ( effect )

import Darcs.Patch.V1.Commute ()
import Darcs.Patch.V1.Core ( Patch(..) )

import Darcs.Witnesses.Ordered ( mapFL_FL )

#include "gadts.h"

instance PrimPatch prim => Apply (Patch prim) where
    apply p = applyPrimFL $ effect p

instance PrimPatch prim => RepairToFL (Patch prim) where
    applyAndTryToFixFL (PP x) = mapMaybeSnd (mapFL_FL PP) `fmap` applyAndTryToFixFL x
    applyAndTryToFixFL x = do apply x; return Nothing
