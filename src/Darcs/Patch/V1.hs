{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1 ( Patch ) where

import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.RepoPatch ( RepoPatch )

import Darcs.Patch.V1.Apply ()
import Darcs.Patch.V1.Commute ()
import Darcs.Patch.V1.Core ( Patch )
import Darcs.Patch.V1.Read ()
import Darcs.Patch.V1.Show ()
import Darcs.Patch.V1.Viewing ()

instance PrimPatch prim => Patchy (Patch prim)
instance PrimPatch prim => RepoPatch (Patch prim)
