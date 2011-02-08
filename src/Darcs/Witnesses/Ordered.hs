-- Copyright (C) 2007 David Roundy
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

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Witnesses.Ordered ( (:>)(..), (:<)(..), (:\/:)(..), (:/\:)(..), (:||:)(..),
                             FL(..), RL(..),
                             lengthFL, mapFL, mapFL_FL, spanFL, foldlFL, allFL, anyFL,
                             filterFL,
                             splitAtFL, splitAtRL, bunchFL, foldlRL,
                             lengthRL, isShorterThanRL, mapRL, mapRL_RL, zipWithFL,
                             filterFLFL,
                             filterRL,
                             reverseFL, reverseRL, (+>+), (+<+),
                             nullFL, concatFL, concatRL,
                             consRLSealed, nullRL, toFL,
                             dropWhileFL, dropWhileRL,
                             spanFL_M,
                             eqFL, eqFLRev, eqFLUnsafe
                           ) where

#include "impossible.h"
import Darcs.Witnesses.Show
import Darcs.Witnesses.Sealed ( FlippedSeal(..), flipSeal, Sealed(..), FreeLeft, unFreeLeft, Sealed2(..),
                                seal )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )

data (a1 :> a2) C(x y) = FORALL(z) (a1 C(x z)) :> (a2 C(z y))
infixr 1 :>
data (a1 :< a2) C(x y) = FORALL(z) (a1 C(z y)) :< (a2 C(x z))
infix 1 :<
infix 1 :/\:, :\/:, :||:
data (a1 :\/: a2) C(x y) = FORALL(z) (a1 C(z x)) :\/: (a2 C(z y))
data (a1 :/\: a2) C(x y) = FORALL(z) (a1 C(x z)) :/\: (a2 C(y z))
data (a1 :||: a2) C(x y) = (a1 C(x y)) :||: (a2 C(x y))

instance (Show2 a, Show2 b) => Show ( (a :> b) C(x y) ) where
    showsPrec d (x :> y) = showOp2 1 ":>" d x y

instance (MyEq a, MyEq b) => MyEq (a :> b) where
    (a1 :> b1) =\/= (a2 :> b2) | IsEq <- a1 =\/= a2 = b1 =\/= b2
                               | otherwise = NotEq

instance (MyEq a, MyEq b) => Eq ((a :> b) C(x y)) where
    (==) = unsafeCompare

instance (MyEq a, MyEq b) => MyEq (a :< b) where
    (a1 :< b1) =\/= (a2 :< b2) | IsEq <- b1 =\/= b2 = a1 =\/= a2
                               | otherwise = NotEq

instance (MyEq a, MyEq b) => Eq ((a :< b) C(x y)) where
    (==) = unsafeCompare

instance (Show2 a, Show2 b) => Show2 (a :> b) where
    showDict2 = ShowDictClass

instance (Show2 a, Show2 b) => Show ( (a :\/: b) C(x y) ) where
    showsPrec d (x :\/: y) = showOp2 9 ":\\/:" d x y

instance (Show2 a, Show2 b) => Show2 (a :\/: b) where
    showDict2 = ShowDictClass

infixr 5 :>:, :<:, +>+, +<+

-- forward list
data FL a C(x z) where
    (:>:) :: a C(x y) -> FL a C(y z) -> FL a C(x z)
    NilFL :: FL a C(x x)

instance Show2 a => Show (FL a C(x z)) where
   showsPrec _ NilFL = showString "NilFL"
   showsPrec d (x :>: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :>: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show1 (FL a C(x)) where
   showDict1 = ShowDictClass

instance Show2 a => Show2 (FL a) where
   showDict2 = ShowDictClass

instance Show2 a => Show (RL a C(x z)) where
   showsPrec _ NilRL = showString "NilRL"
   showsPrec d (x :<: xs) = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                            showString " :<: " . showsPrec (prec + 1) xs
       where prec = 5

instance Show2 a => Show1 (RL a C(x)) where
   showDict1 = ShowDictClass

instance Show2 a => Show2 (RL a) where
   showDict2 = ShowDictClass

-- reverse list
data RL a C(x z) where
    (:<:) :: a C(y z) -> RL a C(x y) -> RL a C(x z)
    NilRL :: RL a C(x x)

nullFL :: FL a C(x z) -> Bool
nullFL NilFL = True
nullFL _ = False

nullRL :: RL a C(x z) -> Bool
nullRL NilRL = True
nullRL _ = False

filterFLFL :: (FORALL(x y) p C(x y) -> EqCheck C(x y)) -> FL p C(w z) -> FL p C(w z)
filterFLFL _ NilFL = NilFL
filterFLFL f (x:>:xs) | IsEq <- f x = filterFLFL f xs
                    | otherwise = x :>: filterFLFL f xs

filterRL :: (FORALL(x y) p C(x y) -> Bool) -> RL p C(a b) ->  [Sealed2 p]
filterRL _ NilRL = []
filterRL f (x :<: xs) | f x = Sealed2 x : (filterRL f xs)
                      | otherwise = filterRL f xs

(+>+) :: FL a C(x y) -> FL a C(y z) -> FL a C(x z)
NilFL +>+ ys = ys
(x:>:xs) +>+ ys = x :>: xs +>+ ys

(+<+) :: RL a C(y z) -> RL a C(x y) -> RL a C(x z)
NilRL +<+ ys = ys
(x:<:xs) +<+ ys = x :<: xs +<+ ys

reverseFL :: FL a C(x z) -> RL a C(x z)
reverseFL xs = r NilRL xs
  where r :: RL a C(l m) -> FL a C(m o) -> RL a C(l o)
        r ls NilFL = ls
        r ls (a:>:as) = r (a:<:ls) as

reverseRL :: RL a C(x z) -> FL a C(x z)
reverseRL xs = r NilFL xs -- r (xs :> NilFL)
  where r :: FL a C(m o) -> RL a C(l m) -> FL a C(l o)
        r ls NilRL = ls
        r ls (a:<:as) = r (a:>:ls) as

concatFL :: FL (FL a) C(x z) -> FL a C(x z)
concatFL NilFL = NilFL
concatFL (a:>:as) = a +>+ concatFL as

concatRL :: RL (RL a) C(x z) -> RL a C(x z)
concatRL NilRL = NilRL
concatRL (a:<:as) = a +<+ concatRL as

spanFL :: (FORALL(w y) a C(w y) -> Bool) -> FL a C(x z) -> (FL a :> FL a) C(x z)
spanFL f (x:>:xs) | f x = case spanFL f xs of
                            ys :> zs -> (x:>:ys) :> zs
spanFL _ xs = NilFL :> xs

spanFL_M :: forall a m C(x z). Monad m =>
            (FORALL(w y) a C(w y) -> m Bool) -> FL a C(x z)
            -> m ((FL a :> FL a) C(x z))
spanFL_M f (x:>:xs) =
    do
      continue <- f x
      if continue
       then do (ys :> zs) <- spanFL_M f xs
               return $ (x :>: ys) :> zs
       else return $ NilFL :> (x :>: xs)

spanFL_M _ (NilFL) = return $ NilFL :> NilFL

splitAtFL :: Int -> FL a C(x z) -> (FL a :> FL a) C(x z)
splitAtFL 0 xs = NilFL :> xs
splitAtFL _ NilFL = NilFL :> NilFL
splitAtFL n (x:>:xs) = case splitAtFL (n-1) xs of
                       (xs':>xs'') -> (x:>:xs' :> xs'')

splitAtRL :: Int -> RL a C(x z) -> (RL a :< RL a) C(x z)
splitAtRL 0 xs = NilRL :< xs
splitAtRL _ NilRL = NilRL :< NilRL
splitAtRL n (x:<:xs) = case splitAtRL (n-1) xs of
                       (xs':<xs'') -> (x:<:xs' :< xs'')

-- 'bunchFL n' groups patches into batches of n, except that it always puts
-- the first patch in its own group, this being a recognition that the
-- first patch is often *very* large.

bunchFL :: Int -> FL a C(x y) -> FL (FL a) C(x y)
bunchFL _ NilFL = NilFL
bunchFL n (x:>:xs) = (x :>: NilFL) :>: bFL xs
    where bFL :: FL a C(x y) -> FL (FL a) C(x y)
          bFL NilFL = NilFL
          bFL bs = case splitAtFL n bs of
                   a :> b -> a :>: bFL b


allFL :: (FORALL(x y) a C(x y) -> Bool) -> FL a C(w z) -> Bool
allFL f xs = and $ mapFL f xs

anyFL :: (FORALL(x y) a C(x y) -> Bool) -> FL a C(w z) -> Bool
anyFL f xs = or $ mapFL f xs

foldlFL :: (FORALL(w y) a -> b C(w y) -> a) -> a -> FL b C(x z) -> a
foldlFL _ x NilFL = x
foldlFL f x (y:>:ys) = foldlFL f (f x y) ys

foldlRL :: (FORALL(w y) a -> b C(w y) -> a) -> a -> RL b C(x z) -> a
foldlRL _ x NilRL = x
foldlRL f x (y:<:ys) = foldlRL f (f x y) ys

mapFL_FL :: (FORALL(w y) a C(w y) -> b C(w y)) -> FL a C(x z) -> FL b C(x z)
mapFL_FL _ NilFL = NilFL
mapFL_FL f (a:>:as) = f a :>: mapFL_FL f as

zipWithFL :: (FORALL(x y) a -> p C(x y) -> q C(x y))
          -> [a] -> FL p C(w z) -> FL q C(w z)
zipWithFL f (x:xs) (y :>: ys) = f x y :>: zipWithFL f xs ys
zipWithFL _ _ NilFL = NilFL
zipWithFL _ [] (_:>:_) = bug "zipWithFL called with too short a list"

mapRL_RL :: (FORALL(w y) a C(w y) -> b C(w y)) -> RL a C(x z) -> RL b C(x z)
mapRL_RL _ NilRL = NilRL
mapRL_RL f (a:<:as) = f a :<: mapRL_RL f as

mapFL :: (FORALL(w z) a C(w z) -> b) -> FL a C(x y) -> [b]
mapFL _ NilFL = []
mapFL f (a :>: b) = f a : mapFL f b

filterFL :: (FORALL(x y) a C(x y) -> Bool) -> FL a C(w z) -> [Sealed2 a]
filterFL _ NilFL = []
filterFL f (a :>: b) = if f a
                       then (Sealed2 a):(filterFL f b)
                       else filterFL f b

mapRL :: (FORALL(w z) a C(w z) -> b) -> RL a C(x y) -> [b]
mapRL _ NilRL = []
mapRL f (a :<: b) = f a : mapRL f b

lengthFL :: FL a C(x z) -> Int
lengthFL xs = l xs 0
  where l :: FL a C(x z) -> Int -> Int
        l NilFL n = n
        l (_:>:as) n = l as $! n+1

lengthRL :: RL a C(x z) -> Int
lengthRL xs = l xs 0
  where l :: RL a C(x z) -> Int -> Int
        l NilRL n = n
        l (_:<:as) n = l as $! n+1

isShorterThanRL :: RL a C(x y) -> Int -> Bool
isShorterThanRL _ n | n <= 0 = False
isShorterThanRL NilRL _ = True
isShorterThanRL (_:<:xs) n = isShorterThanRL xs (n-1)

consRLSealed :: a C(y z) -> FlippedSeal (RL a) C(y) -> FlippedSeal (RL a) C(z)
consRLSealed a (FlippedSeal as) = flipSeal $ a :<: as

toFL :: [FreeLeft a] -> Sealed (FL a C(x))
toFL [] = Sealed NilFL
toFL (x:xs) = case unFreeLeft x of Sealed y -> case toFL xs of Sealed ys -> Sealed (y :>: ys)

dropWhileFL :: (FORALL(x y) a C(x y) -> Bool) -> FL a C(r v) -> FlippedSeal (FL a) C(v)
dropWhileFL _ NilFL       = flipSeal NilFL
dropWhileFL p xs@(x:>:xs')
          | p x       = dropWhileFL p xs'
          | otherwise = flipSeal xs

dropWhileRL :: (FORALL(x y) a C(x y) -> Bool) -> RL a C(r v) -> Sealed (RL a C(r))
dropWhileRL _ NilRL = seal NilRL
dropWhileRL p xs@(x:<:xs')
          | p x       = dropWhileRL p xs'
          | otherwise = seal xs

-- |Check that two 'FL's are equal element by element.
-- This differs from the 'MyEq' instance for 'FL' which
-- uses commutation.
eqFL :: MyEq a => FL a C(x y) -> FL a C(x z) -> EqCheck C(y z)
eqFL NilFL NilFL = IsEq
eqFL (x:>:xs) (y:>:ys) | IsEq <- x =\/= y, IsEq <- eqFL xs ys = IsEq
eqFL _ _ = NotEq

eqFLRev :: MyEq a => FL a C(x z) -> FL a C(y z) -> EqCheck C(x y)
eqFLRev NilFL NilFL = IsEq
eqFLRev (x:>:xs) (y:>:ys) | IsEq <- eqFLRev xs ys, IsEq <- x =/\= y = IsEq
eqFLRev _ _ = NotEq

eqFLUnsafe :: MyEq a => FL a C(x y) -> FL a C(z w) -> Bool
eqFLUnsafe NilFL NilFL = True
eqFLUnsafe (x:>:xs) (y:>:ys) = unsafeCompare x y && eqFLUnsafe xs ys
eqFLUnsafe _ _ = False