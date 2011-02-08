{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE EmptyDataDecls #-}
module Darcs.Patch.Dummy ( DummyPatch ) where

import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Patchy
    ( Patchy, ShowPatch, Invert, Commute, Apply, PatchInspect
    , ReadPatch )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Witnesses.Eq ( MyEq )

#include "gadts.h"

data DummyPatch C(x y)

instance IsHunk DummyPatch
instance PatchListFormat DummyPatch
instance MyEq DummyPatch
instance Invert DummyPatch
instance PatchInspect DummyPatch
instance ReadPatch DummyPatch
instance ShowPatchBasic DummyPatch
instance ShowPatch DummyPatch
instance Commute DummyPatch
instance Apply DummyPatch
instance Patchy DummyPatch