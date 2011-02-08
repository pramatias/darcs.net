{-# LANGUAGE MagicHash #-}
#include "gadts.h"
module Darcs.Witnesses.Unsafe (
  unsafeCoerceP, unsafeCoercePStart,
  unsafeCoercePEnd, unsafeCoerceP2,
  unsafeCoerceP1
) where

import GHC.Base (unsafeCoerce#)

unsafeCoerceP :: a C(x y) -> a C(b c)
unsafeCoerceP = unsafeCoerce#

unsafeCoercePStart :: a C(x1 y) -> a C(x2 y)
unsafeCoercePStart = unsafeCoerce#

unsafeCoercePEnd :: a C(x y1) -> a C(x y2)
unsafeCoercePEnd = unsafeCoerce#

unsafeCoerceP2 :: t C(w x y z) -> t C(a b c d)
unsafeCoerceP2 = unsafeCoerce#

unsafeCoerceP1 :: a C(x) -> a C(y)
unsafeCoerceP1 = unsafeCoerce#
