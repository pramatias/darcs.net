module Darcs.Patch.Markup ( markupFile ) where

import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.MarkupData ( MarkedUpFile )
import Darcs.Patch.Prim ( PrimPatchBase(..), markupPrim )
import Darcs.Witnesses.Ordered ( FL(..) )

#include "gadts.h"

markupFile :: forall p pi C(x y) . (PrimPatchBase p, Effect p) => pi -> p C(x y)
            -> (FilePath, MarkedUpFile pi) -> (FilePath, MarkedUpFile pi)
markupFile x p = mps (effect p)
    where mps :: FL (PrimOf p) C(a b) -> (FilePath, MarkedUpFile pi) -> (FilePath, MarkedUpFile pi)
          mps NilFL = id
          mps (pp:>:pps) = mps pps . markupPrim x pp

