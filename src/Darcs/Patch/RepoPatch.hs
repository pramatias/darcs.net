module Darcs.Patch.RepoPatch ( RepoPatch ) where

import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Merge ( Merge )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Patchy.Instances ()
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf, FromPrim )
import Darcs.Patch.Repair ( RepairToFL, Check )

class (Patchy p, Merge p, Effect p, IsHunk p, FromPrim p, Conflict p, CommuteNoConflicts p, Check p, RepairToFL p, PatchListFormat p,
       PrimPatchBase p, Patchy (PrimOf p), IsHunk (PrimOf p)
      )
    => RepoPatch p

