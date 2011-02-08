{-# OPTIONS_GHC -fno-warn-orphans #-}

module Darcs.Patch.Patchy.Instances () where

import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Patchy ( Patchy )
import Darcs.Patch.Permutations ()
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Viewing ()
import Darcs.Witnesses.Ordered ( FL, RL )

instance (IsHunk p, PatchListFormat p, Patchy p) => Patchy (FL p)
instance (IsHunk p, PatchListFormat p, Patchy p) => Patchy (RL p)
