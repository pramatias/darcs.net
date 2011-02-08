module Darcs.Patch.Invert
       ( Invert(..), invertFL, invertRL
       )
       where

import Darcs.Witnesses.Ordered ( FL(..), RL(..) )

#include "gadts.h"

class Invert p where
    invert :: p C(x y) -> p C(y x)

invertFL :: Invert p => FL p C(x y) -> RL p C(y x)
invertFL NilFL = NilRL
invertFL (x:>:xs) = invert x :<: invertFL xs

invertRL :: Invert p => RL p C(x y) -> FL p C(y x)
invertRL NilRL = NilFL
invertRL (x:<:xs) = invert x :>: invertRL xs
