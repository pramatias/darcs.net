{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Bracketed.Instances () where

import Darcs.Patch.Bracketed ( Bracketed(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..) )

import Darcs.Witnesses.Ordered ( FL(NilFL), mapFL )

import Printer ( vcat, blueText, ($$) )

#include "gadts.h"

instance ShowPatchBasic p => ShowPatchBasic (Bracketed p) where
    showPatch (Singleton p) = showPatch p
    showPatch (Braced NilFL) = blueText "{" $$ blueText "}"
    showPatch (Braced ps) = blueText "{" $$ vcat (mapFL showPatch ps) $$ blueText "}"
    showPatch (Parens ps) = blueText "(" $$ vcat (mapFL showPatch ps) $$ blueText ")"

-- the ReadPatch instance is defined in Darcs.Patch.Read as it is
-- used as an intermediate form during reading of lists of patches
-- that are specified as ListFormatV1 or ListFormatV2.