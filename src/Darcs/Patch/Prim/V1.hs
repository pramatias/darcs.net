{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1 ( Prim ) where

import Darcs.Patch.Prim.V1.Apply ()
import Darcs.Patch.Prim.V1.Coalesce ()
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core ( Prim )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Patch.Prim.V1.Read ()
import Darcs.Patch.Prim.V1.Show ()

import Darcs.Patch.Prim.Class ( PrimPatch )
import Darcs.Patch.Patchy ( Patchy )

instance PrimPatch Prim
instance Patchy Prim