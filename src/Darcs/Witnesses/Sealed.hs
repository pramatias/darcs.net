-- Copyright (C) 2007 David Roundy, 2009 Ganesh Sittampalam
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, FlexibleInstances #-}

#include "gadts.h"

module Darcs.Witnesses.Sealed ( Sealed(..), seal, unseal, mapSeal,
                      unsafeUnseal, unsafeUnsealFlipped, unsafeUnseal2,
                      Sealed2(..), seal2, unseal2, mapSeal2,
                      FlippedSeal(..), flipSeal, unsealFlipped, mapFlipped,
                      unsealM, liftSM,
                      Gap(..), FreeLeft, unFreeLeft, FreeRight, unFreeRight
                    ) where
import Darcs.Witnesses.Eq ( MyEq, EqCheck(..) )
import Darcs.Witnesses.Show
import Darcs.Witnesses.Eq ( (=\/=) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP1, unsafeCoerceP )

data Sealed a where
    Sealed :: a C(x ) -> Sealed a

seal :: a C(x ) -> Sealed a
seal = Sealed

instance MyEq a => Eq (Sealed (a C(x ))) where
    Sealed x == Sealed y | IsEq <- x =\/= y = True
                         | otherwise = False

data Sealed2 a where
    Sealed2 :: !(a C(x y )) -> Sealed2 a

seal2 :: a C(x y ) -> Sealed2 a
seal2 = Sealed2

data FlippedSeal a C(y) where
    FlippedSeal :: !(a C(x y)) -> FlippedSeal a C(y)

flipSeal :: a C(x y) -> FlippedSeal a C(y)
flipSeal = FlippedSeal

unsafeUnseal :: Sealed a -> a C(x)
unsafeUnseal (Sealed a) = unsafeCoerceP1 a

unsafeUnsealFlipped :: FlippedSeal a C(y) -> a C(x y)
unsafeUnsealFlipped (FlippedSeal a) = unsafeCoerceP a

unsafeUnseal2 :: Sealed2 a -> a C(x y)
unsafeUnseal2 (Sealed2 a) = unsafeCoerceP a

unseal :: (FORALL(x) a C(x ) -> b) -> Sealed a -> b
unseal f x = f (unsafeUnseal x)

-- laziness property:
-- unseal (const True) undefined == True

unsealM :: Monad m => m (Sealed a) -> (FORALL(x) a C(x) -> m b) -> m b
unsealM m1 m2 = do sx <- m1
                   unseal m2 sx

liftSM :: Monad m => (FORALL(x) a C(x) -> b) -> m (Sealed a) -> m b
liftSM f m = do sx <- m
                return (unseal f sx)

mapSeal :: (FORALL(x) a C(x ) -> b C(x )) -> Sealed a -> Sealed b
mapSeal f = unseal (seal . f)

mapFlipped :: (FORALL(x) a C(x y) -> b C(x z)) -> FlippedSeal a C(y) -> FlippedSeal b C(z)
mapFlipped f (FlippedSeal x) = FlippedSeal (f x)

unseal2 :: (FORALL(x y) a C(x y ) -> b) -> Sealed2 a -> b
unseal2 f a = f (unsafeUnseal2 a)

mapSeal2 :: (FORALL(x y) a C(x y ) -> b C(x y )) -> Sealed2 a -> Sealed2 b
mapSeal2 f = unseal2 (seal2 . f)

unsealFlipped :: (FORALL(x y) a C(x y) -> b) -> FlippedSeal a C(z) -> b
unsealFlipped f (FlippedSeal a) = f a

instance Show1 a => Show (Sealed a) where
    showsPrec d (Sealed x) = showParen (d > appPrec) $ showString "Sealed " . showsPrec1 (appPrec + 1) x
instance Show2 a => Show (Sealed2 a) where
    showsPrec d (Sealed2 x) = showParen (d > appPrec) $ showString "Sealed2 " . showsPrec2 (appPrec + 1) x

-- |'Poly' is similar to 'Sealed', but the type argument is
-- universally quantified instead of being existentially quantified.
newtype Poly a = Poly { unPoly :: FORALL(x) a C(x) }

-- |'Stepped' is a type level composition operator.
-- For example, 'Stepped Sealed p' is equivalent to 'lambda x . Sealed (p x)'
newtype Stepped f a C(x) = Stepped { unStepped :: f (a C(x)) }

-- |'FreeLeft p' is '\forall x . \exists y . p x y'
-- In other words the caller is free to specify the left witness,
-- and then the right witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that 'y' is dependent on the 'x' that is supplied.
-- This is why 'Stepped' is needed, rather than writing the more obvious
-- 'Sealed (Poly p)' which would notionally have the same quantification
-- of the type witnesses.
newtype FreeLeft p = FLInternal (Poly (Stepped Sealed p))

-- |'FreeLeft p' is '\forall y . \exists x . p x y'
-- In other words the caller is free to specify the right witness,
-- and then the left witness is an existential.
-- Note that the order of the type constructors is important for ensuring
-- that 'x' is dependent on the 'y' that is supplied.
newtype FreeRight p = FRInternal (Poly (FlippedSeal p))

-- |Unwrap a 'FreeLeft' value
unFreeLeft :: FreeLeft p -> Sealed (p C(x))
unFreeLeft (FLInternal x) = unStepped (unPoly x)

-- |Unwrap a 'FreeRight' value
unFreeRight :: FreeRight p -> FlippedSeal p C(x)
unFreeRight (FRInternal x) = unPoly x

-- |'Gap' abstracts over 'FreeLeft' and 'FreeRight' for code constructing these values
class Gap w where
  -- |An empty 'Gap', e.g. 'NilFL' or 'NilRL'
  emptyGap :: (FORALL(x) p C(x x)) -> w p
  -- |A 'Gap' constructed from a completely polymorphic value, for example the constructors
  -- for primitive patches
  freeGap :: (FORALL(x y) p C(x y)) -> w p

  -- |Compose two 'Gap' values together in series, e.g. 'joinGap (+>+)' or 'joinGap (:>:)'
  joinGap :: (FORALL(x y z) p C(x y) -> q C(y z) -> r C(x z)) -> w p -> w q -> w r

instance Gap FreeLeft where
  emptyGap e = FLInternal (Poly (Stepped (Sealed e)))
  freeGap e =  FLInternal (Poly (Stepped (Sealed e)))
  joinGap op (FLInternal p) (FLInternal q)
    = FLInternal (Poly (case unPoly p of Stepped (Sealed p') -> case unPoly q of Stepped (Sealed q') -> Stepped (Sealed (p' `op` q'))))

instance Gap FreeRight where
  emptyGap e = FRInternal (Poly (FlippedSeal e))
  freeGap e =  FRInternal (Poly (FlippedSeal e))
  joinGap op (FRInternal p) (FRInternal q)
    = FRInternal (Poly (case unPoly q of FlippedSeal q' -> case unPoly p of FlippedSeal p' -> FlippedSeal (p' `op` q')))