\begin{code}
module Darcs.Patch.V1.Core
    ( Patch(..),
      isMerger, mergerUndo
    ) where

import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(ListFormatV1) )
import Darcs.Patch.Prim ( FromPrim(..), PrimOf, PrimPatchBase, PrimPatch )
import Darcs.Patch.Repair ( Check )

import Darcs.Witnesses.Ordered ( FL(..), RL )

#include "gadts.h"
#include "impossible.h"

data Patch prim C(x y) where
    PP :: prim C(x y) -> Patch prim C(x y)
    Merger :: FL (Patch prim) C(x y)
           -> RL (Patch prim) C(x b)
           -> Patch prim C(c b)
           -> Patch prim C(c d)
           -> Patch prim C(x y)
    Regrem :: FL (Patch prim) C(x y)
           -> RL (Patch prim) C(x b)
           -> Patch prim C(c b)
           -> Patch prim C(c a)
           -> Patch prim C(y x)

instance PrimPatch prim => PrimPatchBase (Patch prim) where
    type PrimOf (Patch prim) = prim

instance FromPrim (Patch prim) where
    fromPrim = PP

isMerger :: Patch prim C(a b) -> Bool
isMerger (Merger _ _ _ _) = True
isMerger (Regrem _ _ _ _) = True
isMerger _ = False

mergerUndo :: Patch prim C(x y) -> FL (Patch prim) C(x y)
mergerUndo (Merger undo _ _ _) = undo
mergerUndo _ = impossible

instance PatchListFormat (Patch prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect Patch to be used with any other argument
   -- anyway, so it doesn't matter.
   patchListFormat = ListFormatV1

instance Check (Patch prim)
   -- no checks

\end{code}