module Darcs.Patch.Bracketed
    ( Bracketed(..), mapBracketed, unBracketed
    , BracketedFL, mapBracketedFL_FL, unBracketedFL
    ) where

#include "gadts.h"

import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Witnesses.Ordered ( FL(..), mapFL_FL, concatFL )

-- |This type exists for legacy support of on-disk format patch formats.
-- It is a wrapper type that explicitly tracks the nesting of braces and parens
-- in the on-disk representation of such patches. It is used as an intermediate
-- form when reading such patches normally, and also for round-tripping such
-- patches when checking the hash in bundles.
-- It shouldn't be used for anything else.
data Bracketed p C(x y) where
  Singleton :: p C(x y) -> Bracketed p C(x y)            -- A single patch, not wrapped in anything
  Braced :: BracketedFL p C(x y) -> Bracketed p C(x y)   -- A list of patches, wrapped in {}
  Parens :: BracketedFL p C(x y) -> Bracketed p C(x y)   -- A list of patches, wrapped in ()

type BracketedFL p C(x y) = FL (Bracketed p) C(x y)

unBracketed :: Bracketed p C(x y) -> FL p C(x y)
unBracketed (Singleton p) = p :>: NilFL
unBracketed (Braced ps) = unBracketedFL ps
unBracketed (Parens ps) = unBracketedFL ps

unBracketedFL :: BracketedFL p C(x y) -> FL p C(x y)
unBracketedFL = concatFL . mapFL_FL unBracketed

mapBracketed :: (FORALL(a b) p C(a b) -> q C(a b)) -> Bracketed p C(x y) -> Bracketed q C(x y)
mapBracketed f (Singleton p) = Singleton (f p)
mapBracketed f (Braced ps) = Braced (mapBracketedFL_FL f ps)
mapBracketed f (Parens ps) = Parens (mapBracketedFL_FL f ps)

mapBracketedFL_FL :: (FORALL(a b) p C(a b) -> q C(a b)) -> BracketedFL p C(x y) -> BracketedFL q C(x y)
mapBracketedFL_FL f ps = mapFL_FL (mapBracketed f) ps

instance PatchListFormat (Bracketed p)