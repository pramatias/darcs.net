-- Copyright (C) 2002-2003 David Roundy
-- Copyright (C) 2009 Ganesh Sittampalam
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.Permutations ( removeFL, removeRL, removeCommon,
                                  commuteWhatWeCanFL, commuteWhatWeCanRL,
                                  genCommuteWhatWeCanRL,
                                  partitionFL, partitionRL,
                                  simpleHeadPermutationsFL, headPermutationsRL,
                                  headPermutationsFL,
                                  removeSubsequenceFL, removeSubsequenceRL,
                                  partitionConflictingFL,
                                  CommuteFn, selfCommuter,
                                  commuterIdFL, commuterFLId,
                                  commuterIdRL
                                ) where

import Data.Maybe ( mapMaybe )
import Darcs.Patch.Commute ( Commute, commute, commuteFLorComplain, commuteRL )
import Darcs.Patch.Invert ( Invert(..), invertFL, invertRL )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (+<+)
                               , reverseFL, (+>+), (:\/:)(..), lengthFL
                               , lengthRL, reverseRL )
#include "impossible.h"

-- |split an 'FL' into "left" and "right" lists according to a predicate @p@, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "middle" list; to sum up, we have: @all p left@ and @all (not.p) right@, while
-- midddle is mixed.
partitionFL :: Commute p
            => (FORALL(u v) p C(u v) -> Bool)       -- ^predicate; if true we would like the patch in the "left" list
            -> FL p C(x y)                          -- ^input 'FL'
            -> ((FL p :> FL p :> FL p) C(x y))      -- ^"left", "middle" and "right"

-- optimise by using an accumulating parameter to track all the "right" patches that we've found so far
partitionFL' :: Commute p
             => (FORALL(u v) p C(u v) -> Bool)
             -> RL p C(a b)  -- the "middle" patches found so far
             -> RL p C(b c)  -- the "right" patches found so far
             -> FL p C(c d)
             -> ((FL p :> FL p :> FL p) C(a d))

partitionFL keepleft ps = partitionFL' keepleft NilRL NilRL ps

partitionFL' _ middle right NilFL = (NilFL :> reverseRL middle :> reverseRL right)
partitionFL' keepleft middle right (p :>: ps)
   | keepleft p = case commuteRL (right :> p) of
     Just (p' :> right') -> case commuteRL (middle :> p') of
       Just (p'' :> middle') -> case partitionFL' keepleft middle' right' ps of
         (a :> b :> c) -> (p'' :>: a :> b :> c)
       Nothing -> partitionFL' keepleft (p' :<: middle) right' ps
     Nothing -> case commuteWhatWeCanRL (right :> p) of
       (tomiddle :> p' :> right') -> partitionFL' keepleft (p' :<: tomiddle +<+ middle) right' ps
   | otherwise = partitionFL' keepleft middle (p :<: right) ps


-- |split an 'RL' into "left" and "right" lists according to a predicate, using commutation as necessary.
-- If a patch does satisfy the predicate but cannot be commuted past one that does not satisfy
-- the predicate, it goes in the "left" list.
partitionRL :: Commute p
            => (FORALL(u v) p C(u v) -> Bool)    -- ^predicate; if true we would like the patch in the "right" list
            -> RL p C(x y)                       -- ^input 'RL'
            -> (RL p :> RL p) C(x y)             -- ^"left" and "right" results

-- optimise by using an accumulating parameter to track all the "left" patches that we've found so far
partitionRL' :: Commute p
             => (FORALL(u v) p C(u v) -> Bool)
             -> RL p C(x z)
             -> FL p C(z y)   -- the "left" patches found so far
             -> (RL p :> RL p) C(x y)

partitionRL keepright ps = partitionRL' keepright ps NilFL

partitionRL' _ NilRL qs = reverseFL qs :> NilRL

partitionRL' keepright (p :<: ps) qs
   | keepright p,
     Right (qs' :> p') <- commuteFLorComplain (p :> qs)
       = case partitionRL' keepright ps qs' of
         a :> b -> a :> p' :<: b
   | otherwise = partitionRL' keepright ps (p :>: qs)

commuteWhatWeCanFL :: Commute p => (p :> FL p) C(x y) -> (FL p :> p :> FL p) C(x y)
commuteWhatWeCanFL (p :> x :>: xs) =
    case commute (p :> x) of
    Nothing -> case commuteWhatWeCanFL (x :> xs) of
               xs1 :> x' :> xs2 -> case commuteWhatWeCanFL (p :> xs1) of
                              xs1' :> p' :> xs2' -> xs1' :> p' :> xs2' +>+ x' :>: xs2
    Just (x' :> p') -> case commuteWhatWeCanFL (p' :> xs) of
                       a :> p'' :> c -> x' :>: a :> p'' :> c
commuteWhatWeCanFL (y :> NilFL) = NilFL :> y :> NilFL

commuteWhatWeCanRL :: Commute p => (RL p :> p) C(x y) -> (RL p :> p :> RL p) C(x y)
commuteWhatWeCanRL = genCommuteWhatWeCanRL commute

genCommuteWhatWeCanRL :: (FORALL(a b) ((p :> p) C(a b) -> Maybe ((p :> p) C(a b))))
                      -> (RL p :> p) C(x y) -> (RL p :> p :> RL p) C(x y)
genCommuteWhatWeCanRL com (x :<: xs :> p) =
    case com (x :> p) of
    Nothing -> case genCommuteWhatWeCanRL com (xs :> x) of
               xs1 :> x' :> xs2 -> case genCommuteWhatWeCanRL com (xs2 :> p) of
                              xs1' :> p' :> xs2' -> xs1' +<+ x' :<: xs1 :> p' :> xs2'
    Just (p' :> x') -> case genCommuteWhatWeCanRL com (xs :> p') of
                       a :> p'' :> c -> a :> p'' :> x' :<: c
genCommuteWhatWeCanRL _ (NilRL :> y) = NilRL :> y :> NilRL


removeCommon :: (MyEq p, Commute p) => (FL p :\/: FL p) C(x y) -> (FL p :\/: FL p) C(x y)
removeCommon (xs :\/: NilFL) = xs :\/: NilFL
removeCommon (NilFL :\/: xs) = NilFL :\/: xs
removeCommon (xs :\/: ys) = rc xs (headPermutationsFL ys)
    where rc :: (MyEq p, Commute p) => FL p C(x y) -> [(p:>FL p) C(x z)] -> (FL p :\/: FL p) C(y z)
          rc nms ((n:>ns):_) | Just ms <- removeFL n nms = removeCommon (ms :\/: ns)
          rc ms [n:>ns] = ms :\/: n:>:ns
          rc ms (_:nss) = rc ms nss
          rc _ [] = impossible -- because we already checked for NilFL case

-- | 'removeFL' @x xs@ removes @x@ from @xs@ if @x@ can be commuted to its head.
--   Otherwise it returns 'Nothing'
removeFL :: (MyEq p, Commute p) => p C(x y) -> FL p C(x z) -> Maybe (FL p C(y z))
removeFL x xs = r x $ headPermutationsFL xs
    where r :: (MyEq p, Commute p) => p C(x y) -> [(p:>FL p) C(x z)] -> Maybe (FL p C(y z))
          r _ [] = Nothing
          r z ((z':>zs):zss) | IsEq <- z =\/= z' = Just zs
                             | otherwise = r z zss

-- | 'removeRL' is like 'removeFL' except with 'RL'
removeRL :: (MyEq p, Commute p) => p C(y z) -> RL p C(x z) -> Maybe (RL p C(x y))
removeRL x xs = r x $ headPermutationsRL xs
    where r :: (MyEq p, Commute p) => p C(y z) -> [RL p C(x z)] -> Maybe (RL p C(x y))
          r z ((z':<:zs):zss) | IsEq <- z =/\= z' = Just zs
                              | otherwise = r z zss
          r _ _ = Nothing

-- | 'removeSubsequenceFL' @ab abc@ returns @Just c'@ where all the patches in
--   @ab@ have been commuted out of it, if possible.  If this is not possible
--   for any reason (the set of patches @ab@ is not actually a subset of @abc@,
--   or they can't be commuted out) we return 'Nothing'.
removeSubsequenceFL :: (MyEq p, Commute p) => FL p C(a b)
                     -> FL p C(a c) -> Maybe (FL p C(b c))
removeSubsequenceFL a b | lengthFL a > lengthFL b = Nothing
                         | otherwise = rsFL a b
    where rsFL :: (MyEq p, Commute p) => FL p C(a b) -> FL p C(a c) -> Maybe (FL p C(b c))
          rsFL NilFL ys = Just ys
          rsFL (x:>:xs) yys = removeFL x yys >>= removeSubsequenceFL xs

-- | 'removeSubsequenceRL' is like @removeSubsequenceFL@ except that it works
--   on 'RL'
removeSubsequenceRL :: (MyEq p, Commute p) => RL p C(ab abc)
                     -> RL p C(a abc) -> Maybe (RL p C(a ab))
removeSubsequenceRL a b | lengthRL a > lengthRL b = Nothing
                         | otherwise = rsRL a b
    where rsRL :: (MyEq p, Commute p) => RL p C(ab abc) -> RL p C(a abc) -> Maybe (RL p C(a ab))
          rsRL NilRL ys = Just ys
          rsRL (x:<:xs) yys = removeRL x yys >>= removeSubsequenceRL xs

-- | This is a minor variant of 'headPermutationsFL' with each permutation
--   is simply returned as a 'FL'
simpleHeadPermutationsFL :: Commute p => FL p C(x y) -> [FL p C(x y)]
simpleHeadPermutationsFL ps = map (\ (x:>xs) -> x:>:xs) $ headPermutationsFL ps

-- | 'headPermutationsFL' @p:>:ps@ returns all the permutations of the list
--   in which one element of @ps@ is commuted past @p@
--
--   Suppose we have a sequence of patches
--
--   >  X h a y s-t-c k
--
--   Suppose furthermore that the patch @c@ depends on @t@, which in turn
--   depends on @s@.  This function will return
--
--   > X :> h a y s t c k
--   > h :> X a y s t c k
--   > a :> X h y s t c k
--   > y :> X h a s t c k
--   > s :> X h a y t c k
--   > k :> X h a y s t c
headPermutationsFL :: Commute p => FL p C(x y) -> [(p :> FL p) C(x y)]
headPermutationsFL NilFL = []
headPermutationsFL (p:>:ps) =
    (p:>ps) : mapMaybe (swapfirstFL.(p:>)) (headPermutationsFL ps)
        where swapfirstFL (p1:>p2:>xs) = do p2':>p1' <- commute (p1:>p2)
                                            Just $ p2':>p1':>:xs

-- | 'headPermutationsRL' is like 'headPermutationsFL', except that we
--   operate on an 'RL' (in other words, we are pushing things to the end of a
--   patch sequence instead of to the beginning).
headPermutationsRL :: Commute p => RL p C(x y) -> [RL p C(x y)]
headPermutationsRL NilRL = []
headPermutationsRL (p:<:ps) =
    (p:<:ps) : mapMaybe (swapfirstRL.(p:<:)) (headPermutationsRL ps)
        where swapfirstRL (p1:<:p2:<:xs) = do p1':>p2' <- commute (p2:>p1)
                                              Just $ p2':<:p1':<:xs
              swapfirstRL _ = Nothing

instance (MyEq p, Commute p) => MyEq (FL p) where
    a =\/= b | lengthFL a /= lengthFL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: FL p C(x y) -> FL p C(x z) -> EqCheck C(y z)
                   cmpSameLength (x:>:xs) xys | Just ys <- removeFL x xys = cmpSameLength xs ys
                   cmpSameLength NilFL NilFL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =/\= ys = reverseFL xs =/\= reverseFL ys

instance (Invert p, Commute p) => Invert (FL p) where
    invert = reverseRL . invertFL

instance (MyEq p, Commute p) => MyEq (RL p) where
    unsafeCompare = bug "Buggy use of unsafeCompare on RL"
    a =/\= b | lengthRL a /= lengthRL b = NotEq
             | otherwise = cmpSameLength a b
             where cmpSameLength :: RL p C(x y) -> RL p C(w y) -> EqCheck C(x w)
                   cmpSameLength (x:<:xs) xys | Just ys <- removeRL x xys = cmpSameLength xs ys
                   cmpSameLength NilRL NilRL = IsEq
                   cmpSameLength _ _ = NotEq
    xs =\/= ys = reverseRL xs =\/= reverseRL ys

instance (Commute p, Invert p) => Invert (RL p) where
    invert = reverseFL . invertRL

-- |CommuteFn is the basis of a general framework for building up commutation
-- operations between different patch types in a generic manner. Unfortunately
-- type classes are not well suited to the problem because of the multiple possible
-- routes by which the commuter for (FL p1, FL p2) can be built out of the
-- commuter for (p1, p2) - and more complicated problems when we start building
-- multiple constructors on top of each other. The type class resolution machinery
-- really can't cope with selecting some route, because it doesn't know that all
-- possible routes should be equivalent.
type CommuteFn p1 p2 = FORALL(x y) (p1 :> p2) C(x y) -> Maybe ((p2 :> p1) C(x y))

-- |Build a commuter between a patch and itself using the operation from the type class.
selfCommuter :: Commute p => CommuteFn p p
selfCommuter = commute

commuterIdRL :: CommuteFn p1 p2 -> CommuteFn p1 (RL p2)
commuterIdRL _ (x :> NilRL) = return (NilRL :> x)
commuterIdRL commuter (x :> (y :<: ys))
  = do ys' :> x' <- commuterIdRL commuter (x :> ys)
       y' :> x'' <- commuter (x' :> y)
       return ((y' :<: ys') :> x'')

commuterIdFL :: CommuteFn p1 p2 -> CommuteFn p1 (FL p2)
commuterIdFL _ (x :> NilFL) = return (NilFL :> x)
commuterIdFL commuter (x :> (y :>: ys))
  = do y' :> x' <- commuter (x :> y)
       ys' :> x'' <- commuterIdFL commuter (x' :> ys)
       return ((y' :>: ys') :> x'')

commuterFLId :: CommuteFn p1 p2 -> CommuteFn (FL p1) p2
commuterFLId _ (NilFL :> y) = return (y :> NilFL)
commuterFLId commuter ((x :>: xs) :> y)
  = do y' :> xs' <- commuterFLId commuter (xs :> y)
       y'' :> x' <- commuter (x :> y')
       return (y'' :> (x' :>: xs'))

-- |Partition a list into the patches that commute with the given patch and those that don't (including dependencies)
partitionConflictingFL :: (Commute p1, Invert p1) => CommuteFn p1 p2 -> FL p1 C(x y) -> p2 C(x z) -> (FL p1 :> FL p1) C(x y)
partitionConflictingFL _ NilFL _ = (NilFL :> NilFL)
partitionConflictingFL commuter (x :>: xs) y =
   case commuter (invert x :> y) of
     Nothing -> case commuteWhatWeCanFL (x :> xs) of
                 xs_ok :> x' :> xs_deps ->
                   case partitionConflictingFL commuter xs_ok y of
                     xs_clean :> xs_conflicts -> xs_clean :> (xs_conflicts +>+ (x' :>: xs_deps))
     Just (y' :> _) ->
       case partitionConflictingFL commuter xs y' of
          xs_clean :> xs_conflicts -> (x :>: xs_clean) :> xs_conflicts
