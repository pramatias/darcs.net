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

{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}
{-# LANGUAGE CPP, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, NoMonomorphismRestriction #-}

#include "gadts.h"

module Darcs.Test.Patch.Unit ( patchUnitTests ) where

import Data.Maybe ( catMaybes, isNothing )
import qualified Data.ByteString.Char8 as BC ( pack )
import Darcs.Witnesses.Sealed
import Darcs.Patch ( Patchy, invert, showPatch, hunk )
import qualified Darcs.Patch as W ( commute )
import Darcs.Patch.Merge ( Merge )
import qualified Darcs.Patch.Merge as W ( merge, mergeFL )
import Darcs.Patch.Patchy ( Commute, Invert(..) )
import Darcs.Patch.Prim ( PrimPatch )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.V2.Real ( prim2real, isConsistent, isForward )
-- import Darcs.Test.Patch.Test () -- for instance Eq Patch
import qualified Darcs.Test.Patch.Properties as W
     ( permutivity, partialPermutivity
     , mergeConsistent, mergeArgumentsConsistent, mergeEitherWay
     , mergeCommute, patchAndInverseCommute, joinCommute, commuteInverses
     , recommute
     , show_read
     )
import qualified Darcs.Test.Patch.QuickCheck as W
     ( getPairs, getTriples )
import qualified Darcs.Test.Patch.Examples2 as W
     ( mergeExamples, commuteExamples, tripleExamples
     , realPatchLoopExamples
     )
import Darcs.Test.Patch.QuickCheck
     ( WithStartState, RepoModel, Tree
     , propConsistentTreeFlattenings
     )
import Darcs.Witnesses.Eq
import qualified Darcs.Witnesses.Ordered as W
import Darcs.Witnesses.Show
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart, unsafeCoercePEnd )
import qualified Darcs.Test.Patch.Unit2 as W ( notDuplicatestriple )
import qualified Darcs.Patch.Prim as W ( join )
import Printer ( Doc, redText, ($$) )
--import Printer ( greenText )
--import Darcs.ColorPrinter ( traceDoc )
--import Darcs.ColorPrinter ( errorDoc )
import Darcs.ColorPrinter () -- for instance Show Doc
import Test.HUnit ( assertBool )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework ( Test )

-- import Debug.Trace
-- #include "impossible.h"

{-
This module builds a lot of test cases by pattern matching
on the results of merge/commute in where clauses. This would
be very painful to switch to using witnesses properly, because
we'd have to make them use case in series.

So instead we give up on witnesses for this module, but instead
of preprocessor hacks which make incompatible code with the rest
of darcs, we build a fresh set of witnesses constructors (FL etc)
which aren't actually GADTs or existentials. So the pattern matching
works as before, but we need to translate back and forth a lot.

We call the normal darcs constructors the 'W' variants.
-}

infixr 5 :>:
infixr 5 +>+
infixr 1 :>
infix 1 :/\:
infix 1 :\/:

data FL p C(x y) where
   NilFL :: FL p C(x y)
   (:>:) :: p C(x y) -> FL p C(x y) -> FL p C(x y)

(+>+) :: FL p C(x y) -> FL p C(x y) -> FL p C(x y)
NilFL +>+ ps = ps
(p :>: ps) +>+ qs = p :>: (ps +>+ qs)

data (p :> q) C(x y) where
   (:>) :: p C(x y) -> q C(x y) -> (p :> q) C(x y)

data (p :\/: q) C(x y) where
   (:\/:) :: p C(x y) -> q C(x y) -> (p :\/: q) C(x y)

data (p :/\: q) C(x y) where
   (:/\:) :: p C(x y) -> q C(x y) -> (p :/\: q) C(x y)

class WSub wp p | p -> wp, wp -> p where
   fromW :: wp C(x y) -> p C(x y)
   toW :: p C(x y) -> wp C(x y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:\/: wp2) (p1 :\/: p2) where
   fromW (x W.:\/: y) = unsafeCoerceP (fromW x) :\/: unsafeCoerceP (fromW y)
   toW (x :\/: y) = unsafeCoerceP (toW x) W.:\/: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:/\: wp2) (p1 :/\: p2) where
   fromW (x W.:/\: y) = unsafeCoerceP (fromW x) :/\: unsafeCoerceP (fromW y)
   toW (x :/\: y) = unsafeCoerceP (toW x) W.:/\: unsafeCoerceP (toW y)

instance (WSub wp1 p1, WSub wp2 p2) => WSub (wp1 W.:> wp2) (p1 :> p2) where
   fromW (x W.:> y) = unsafeCoercePEnd (fromW x) :> unsafeCoercePStart (fromW y)
   toW (x :> y) = unsafeCoercePEnd (toW x) W.:> unsafeCoercePStart (toW y)

instance WSub wp p => WSub (W.FL wp) (FL p) where
   fromW W.NilFL = unsafeCoerceP NilFL
   fromW (x W.:>: xs) = unsafeCoercePEnd (fromW x) :>: unsafeCoercePStart (fromW xs)

   toW NilFL = unsafeCoerceP W.NilFL
   toW (x :>: xs) = unsafeCoercePEnd (toW x) W.:>: unsafeCoercePStart (toW xs)

instance WSub prim prim => WSub (RealPatch prim) (RealPatch prim) where
   fromW = id
   toW = id

instance WSub Prim Prim where
   fromW = id
   toW = id

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :> q) C(x y)) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :> q) where
   showDict2 = ShowDictClass

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show ((p :\/: q) C(x y)) where
   show = show . toW

instance (WSub wp p, WSub wq q, Show2 wp, Show2 wq) => Show2 (p :\/: q) where
   showDict2 = ShowDictClass

instance (WSub wp p, Show2 wp) => Show (FL p C(x y)) where
   show = show . toW

instance (WSub wp p, Show2 wp) => Show2 (FL p) where
   showDict2 = ShowDictClass

instance (WSub wp p, Commute wp, MyEq wp) => MyEq (FL p) where
   unsafeCompare x y = unsafeCompare (toW x) (toW y)

instance (WSub wp p, Commute wp, Invert wp) => Invert (FL p) where
   invert = fromW . invert . toW

instance (WSub wp p, Commute wp) => Commute (FL p) where
   commute (xs W.:> ys) = do ys' W.:> xs' <- W.commute (toW xs W.:> toW ys)
                             return (fromW ys' W.:> fromW xs')

mergeFL :: (WSub wp p, Merge wp) => (p :\/: FL p) C(x y) -> (FL p :/\: p) C(x y)
mergeFL = fromW . W.mergeFL . toW

merge :: (WSub wp p, Merge wp) => (p :\/: p) C(x y) -> (p :/\: p) C(x y)
merge = fromW . W.merge . toW

commute :: (WSub wp p, Commute wp) => (p :> p) C(x y) -> Maybe ((p :> p) C(x y))
commute = fmap fromW . W.commute . toW


getPairs :: FL (RealPatch Prim) C(x y) -> [Sealed2 (RealPatch Prim :> RealPatch Prim)]
getPairs = map (mapSeal2 fromW) . W.getPairs . toW

getTriples :: FL (RealPatch Prim) C(x y) -> [Sealed2 (RealPatch Prim :> RealPatch Prim :> RealPatch Prim)]
getTriples = map (mapSeal2 fromW) . W.getTriples . toW

mergeExamples :: [Sealed2 (RealPatch Prim :\/: RealPatch Prim)]
mergeExamples = map (mapSeal2 fromW) W.mergeExamples

realPatchLoopExamples :: [Sealed (WithStartState RepoModel (Tree Prim))]
realPatchLoopExamples = W.realPatchLoopExamples

commuteExamples :: [Sealed2 (RealPatch Prim :> RealPatch Prim)]
commuteExamples = map (mapSeal2 fromW) W.commuteExamples

tripleExamples :: [Sealed2 (RealPatch Prim :> RealPatch Prim :> RealPatch Prim)]
tripleExamples = map (mapSeal2 fromW) W.tripleExamples

join :: (Prim :> Prim) C(x y) -> Maybe (FL Prim C(x y))
join = fmap fromW . W.join . toW

joinCommute :: (FORALL(x y) (Prim :> Prim) C(x y) -> Maybe (FL Prim C(x y)))
             -> (Prim :> Prim :> Prim) C(a b) -> Maybe Doc
joinCommute f = W.joinCommute (fmap toW . f . fromW) . toW

permutivity :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
            -> (p :> p :> p) C(a b) -> Maybe Doc
permutivity f = W.permutivity (fmap toW . f . fromW) . toW

partialPermutivity :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                    -> (p :> p :> p) C(a b) -> Maybe Doc
partialPermutivity f = W.partialPermutivity (fmap toW . f . fromW) . toW

mergeEitherWay :: (Patchy wp, Merge wp, WSub wp p) => (p :\/: p) C(x y) -> Maybe Doc
mergeEitherWay = W.mergeEitherWay . toW

commuteInverses :: (Patchy wp, WSub wp p) => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                 -> (p :> p) C(a b) -> Maybe Doc
commuteInverses f = W.commuteInverses (fmap toW . f . fromW) . toW

recommute :: (Patchy wp, WSub wp p) => (FORALL(x y) ((p :> p) C(x y) -> Maybe ((p :> p) C(x y))))
          -> (p :> p) C(a b) -> Maybe Doc
recommute f = W.recommute (fmap toW . f . fromW) . toW

mergeCommute :: (Patchy wp, Merge wp, WSub wp p) => (p :\/: p) C(x y) -> Maybe Doc
mergeCommute = W.mergeCommute . toW

mergeConsistent :: (Patchy wp, Merge wp, WSub wp p) =>
                           (FORALL(x y) p C(x y) -> Maybe Doc)
                        -> (p :\/: p) C(a b) -> Maybe Doc
mergeConsistent f = W.mergeConsistent (f . fromW) . toW

mergeArgumentsConsistent :: (Patchy wp, WSub wp p) =>
                              (FORALL(x y) p C(x y) -> Maybe Doc)
                           -> (p :\/: p) C(a b) -> Maybe Doc
mergeArgumentsConsistent f = W.mergeArgumentsConsistent (f . fromW) . toW

show_read :: (Patchy p, Show2 p) => p C(x y) -> Maybe Doc
show_read = W.show_read

patchAndInverseCommute :: (Patchy wp, WSub wp p) =>
                             (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                          -> (p :> p) C(a b) -> Maybe Doc
patchAndInverseCommute f = W.patchAndInverseCommute (fmap toW . f . fromW) . toW

notDuplicatestriple :: (RealPatch Prim :> RealPatch Prim :> RealPatch Prim) C(x y) -> Bool
notDuplicatestriple = W.notDuplicatestriple . toW

{- end of W<->nonW translation code -}

-- | The unit tests defined about patches
patchUnitTests :: [Test]
patchUnitTests = [
                    runPrimitiveTests "join commute" (joinCommute join) primPermutables,
                    runPrimitiveTests "prim recommute" (recommute commute) $ map mergeable2commutable mergeables,
                    runPrimitiveTests "prim patch and inverse commute" (patchAndInverseCommute commute) $ map mergeable2commutable mergeables,
                    runPrimitiveTests "prim inverses commute" (commuteInverses commute) $ map mergeable2commutable mergeables,
                    runPrimitiveTests "FL prim recommute" (recommute commute) $ map mergeable2commutable mergeablesFL,
                    runPrimitiveTests "FL prim patch and inverse commute" (patchAndInverseCommute commute) $ map mergeable2commutable mergeablesFL,
                    runPrimitiveTests "FL prim inverses commute" (commuteInverses commute) $ map mergeable2commutable mergeablesFL,
                    runPrimitiveTests "fails" (commuteFails commute) ([] :: [(Prim :> Prim) C(x y)]),
                    runPrimitiveTests "read and show work on Prim" show_read primPatches,
                    runPrimitiveTests "read and show work on RealPatch" show_read realPatches,
                    runPrimitiveTests "example flattenings work"
                                        (\x -> if propConsistentTreeFlattenings x
                                                 then Nothing
                                                 else Just $ redText "oops")
                                        realPatchLoopExamples,
                    runPrimitiveTests "real merge input consistent" (mergeArgumentsConsistent isConsistent) realMergeables,
                    runPrimitiveTests "real merge input is forward" (mergeArgumentsConsistent isForward) realMergeables,
                    runPrimitiveTests "real merge output is forward" (mergeConsistent isForward) realMergeables,
                    runPrimitiveTests "real merge output consistent" (mergeConsistent isConsistent) realMergeables,
                    runPrimitiveTests "real merge either way" mergeEitherWay realMergeables,
                    runPrimitiveTests "real merge and commute" mergeCommute realMergeables,

                    runPrimitiveTests "real recommute" (recommute commute) realCommutables,
                    runPrimitiveTests "real inverses commute" (commuteInverses commute) realCommutables,
                    runPrimitiveTests "real permutivity" (permutivity commute) $ filter (notDuplicatestriple) realTriples,
                    runPrimitiveTests "real partial permutivity" (partialPermutivity commute) $ filter (notDuplicatestriple) realTriples

                   ]

-- | Run a test function on a set of data, using HUnit. The test function should
--   return @Nothing@ upon success and a @Just x@ upon failure.
runPrimitiveTests :: (Show a, Show b) => String         -- ^ The test name
                                        -> (a -> Maybe b) -- ^ The test function
                                        -> [a]            -- ^ The test data
                                        -> Test
runPrimitiveTests name test datas = testCase name (assertBool assertName res)
    where assertName = "Boolean assertion for \"" ++ name ++ "\""
          res        = and $ map (isNothing . test) datas

quickhunk :: PrimPatch prim => Int -> String -> String -> prim C(x y)
quickhunk l o n = hunk "test" l (map (\c -> BC.pack [c]) o)
                                (map (\c -> BC.pack [c]) n)

primPermutables :: [(Prim :> Prim :> Prim) C(x y)]
primPermutables =
    [quickhunk 0 "e" "bo" :> quickhunk 3 "" "x" :> quickhunk 2 "f" "qljo"]

mergeables :: [(Prim :\/: Prim) C(x y)]
mergeables = [quickhunk 1 "a" "b" :\/: quickhunk 1 "a" "c",
              quickhunk 1 "a" "b" :\/: quickhunk 3 "z" "c",
              quickhunk 0 "" "a" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "" "b",
              quickhunk 0 "a" "" :\/: quickhunk 1 "b" "",
              quickhunk 0 "" "a" :\/: quickhunk 1 "b" ""
             ]

mergeablesFL :: [(FL Prim :\/: FL Prim) C(x y)]
mergeablesFL = map (\ (x:\/:y) -> (x :>: NilFL) :\/: (y :>: NilFL)) mergeables ++
           [] --    [(quickhunk 1 "a" "b" :>: quickhunk 3 "z" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL),
              --  (quickhunk 1 "a" "b" :>: quickhunk 1 "b" "c" :>: NilFL)
              --  :\/: (quickhunk 1 "a" "z" :>: NilFL)]

mergeable2commutable :: Invert p => (p :\/: p) C(x y) -> (p :> p) C(x y)
mergeable2commutable (x :\/: y) = unsafeCoerceP (invert x) :> y

primPatches :: [Prim C(x y)]
primPatches = concatMap mergeable2patches mergeables
    where mergeable2patches (x:\/:y) = [x,y]

realPatches :: [RealPatch Prim C(x y)]
realPatches = concatMap commutable2patches realCommutables
    where commutable2patches (x:>y) = [x,y]

realTriples :: [(RealPatch Prim :> RealPatch Prim :> RealPatch Prim) C(x y)]
realTriples = [ob' :> oa2 :> a2'',
                oa' :> oa2 :> a2'']
               ++ map unsafeUnseal2 tripleExamples
               ++ map unsafeUnseal2 (concatMap getTriples realFLs)
    where oa = prim2real $ quickhunk 1 "o" "aa"
          oa2 = oa
          a2 = prim2real $ quickhunk 2 "a34" "2xx"
          ob = prim2real $ quickhunk 1 "o" "bb"
          ob' :/\: oa' = merge (oa :\/: ob)
          a2' :/\: _ = merge (ob' :\/: a2)
          a2'' :/\: _ = merge (oa2 :\/: a2')

realFLs :: [FL (RealPatch Prim) C(x y)]
realFLs = [oa :>: invert oa :>: oa :>: invert oa :>: ps +>+ oa :>: invert oa :>: NilFL]
    where oa = prim2real $ quickhunk 1 "o" "a"
          ps :/\: _ = merge (oa :>: invert oa :>: NilFL :\/: oa :>: invert oa :>: NilFL)

realCommutables :: [(RealPatch Prim :> RealPatch Prim) C(x y)]
realCommutables = map unsafeUnseal2 commuteExamples++
                   map mergeable2commutable realMergeables++
                   [invert oa :> ob'] ++ map unsafeUnseal2 (concatMap getPairs realFLs)
    where oa = prim2real $ quickhunk 1 "o" "a"
          ob = prim2real $ quickhunk 1 "o" "b"
          _ :/\: ob' = mergeFL (ob :\/: oa :>: invert oa :>: NilFL)

realMergeables :: [(RealPatch Prim :\/: RealPatch Prim) C(x y)]
realMergeables = map (\ (x :\/: y) -> prim2real x :\/: prim2real y) mergeables
                        ++ realIglooMergeables
                        ++ realQuickcheckMergeables
                        ++ map unsafeUnseal2 mergeExamples
                        ++ catMaybes (map pair2m (concatMap getPairs realFLs))
                        ++ [(oa :\/: od),
                            (oa :\/: a2'),
                            (ob' :\/: od''),
                            (oe :\/: od),
                            (of' :\/: oe'),
                            (ob' :\/: oe'),
                            (oa :\/: oe'),
                            (ob' :\/: oc'),
                            (b2' :\/: oc'''),
                            (ob' :\/: a2),
                            (b2' :\/: og'''),
                            (oc''' :\/: og'''),
                            (oc'' :\/: og''),
                            (ob'' :\/: og''),
                            (ob'' :\/: oc''),
                            (oc' :\/: od'')]
    where oa = prim2real $ quickhunk 1 "o" "aa"
          a2 = prim2real $ quickhunk 2 "a34" "2xx"
          og = prim2real $ quickhunk 3 "4" "g"
          ob = prim2real $ quickhunk 1 "o" "bb"
          b2 = prim2real $ quickhunk 2 "b" "2"
          oc = prim2real $ quickhunk 1 "o" "cc"
          od = prim2real $ quickhunk 7 "x" "d"
          oe = prim2real $ quickhunk 7 "x" "e"
          pf = prim2real $ quickhunk 7 "x" "f"
          od'' = prim2real $ quickhunk 8 "x" "d"
          ob' :>: b2' :>: NilFL :/\: _ = mergeFL (oa :\/: ob :>: b2 :>: NilFL)
          a2' :/\: _ = merge (ob' :\/: a2)
          ob'' :/\: _ = merge (a2 :\/: ob')
          og' :/\: _ = merge (oa :\/: og)
          og'' :/\: _ = merge (a2 :\/: og')
          og''' :/\: _ = merge (ob' :\/: og')
          oc' :/\: _ = merge (oa :\/: oc)
          oc'' :/\: _ = merge (a2 :\/: oc)
          oc''' :/\: _ = merge (ob' :\/: oc')
          oe' :/\: _ = merge (od :\/: oe)
          of' :/\: _ = merge (od :\/: pf)
          pair2m :: Sealed2 (RealPatch Prim :> RealPatch Prim)
                 -> Maybe ((RealPatch Prim :\/: RealPatch Prim) C(x y))
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

realIglooMergeables :: [(RealPatch Prim :\/: RealPatch Prim) C(x y)]
realIglooMergeables = [(a :\/: b),
                    (b :\/: c),
                    (a :\/: c),
                    (x :\/: a),
                    (y :\/: b),
                    (z :\/: c),
                    (x' :\/: y'),
                    (z' :\/: y'),
                    (x' :\/: z'),
                    (a :\/: a)]
    where a = prim2real $ quickhunk 1 "1" "A"
          b = prim2real $ quickhunk 2 "2" "B"
          c = prim2real $ quickhunk 3 "3" "C"
          x = prim2real $ quickhunk 1 "1BC" "xbc"
          y = prim2real $ quickhunk 1 "A2C" "ayc"
          z = prim2real $ quickhunk 1 "AB3" "abz"
          x' :/\: _ = merge (a :\/: x)
          y' :/\: _ = merge (b :\/: y)
          z' :/\: _ = merge (c :\/: z)

realQuickcheckMergeables :: [(RealPatch Prim :\/: RealPatch Prim) C(x y)]
realQuickcheckMergeables = [-- invert k1 :\/: n1
                             --, invert k2 :\/: n2
                               hb :\/: k
                             , b' :\/: b'
                             , n' :\/: n'
                             , b :\/: d
                             , k' :\/: k'
                             , k3 :\/: k3
                             ] ++ catMaybes (map pair2m pairs)
    where hb = prim2real $ quickhunk 0 "" "hb"
          k = prim2real $ quickhunk 0 "" "k"
          n = prim2real $ quickhunk 0 "" "n"
          b = prim2real $ quickhunk 1 "b" ""
          d = prim2real $ quickhunk 2 "" "d"
          d':/\:_ = merge (b :\/: d)
          --k1 :>: n1 :>: NilFL :/\: _ = mergeFL (hb :\/: k :>: n :>: NilFL)
          --k2 :>: n2 :>: NilFL :/\: _ =
          --    merge (hb :>: b :>: NilFL :\/: k :>: n :>: NilFL)
          k' :>: n' :>: NilFL :/\: _ :>: b' :>: _ = merge (hb :>: b :>: d' :>: NilFL :\/: k :>: n :>: NilFL)
          pairs = getPairs (hb :>: b :>: d' :>: k' :>: n' :>: NilFL)
          pair2m :: Sealed2 (RealPatch Prim :> RealPatch Prim)
                 -> Maybe ((RealPatch Prim :\/: RealPatch Prim) C(x y))
          pair2m (Sealed2 (xx :> y)) = do y' :> _ <- commute (xx :> y)
                                          return $ unsafeCoerceP (xx :\/: y')

          i = prim2real $ quickhunk 0 "" "i"
          x = prim2real $ quickhunk 0 "" "x"
          xi = prim2real $ quickhunk 0 "xi" ""
          d3 :/\: _ = merge (xi :\/: d)
          _ :/\: k3 = mergeFL (k :\/: i :>: x :>: xi :>: d3 :>: NilFL)

commuteFails :: (MyEq p, Patchy p)
             => ((p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
             -> (p :> p) C(x y)
             -> Maybe Doc
commuteFails c (x :> y) =  do y' :> x' <- c (x :> y)
                              return $ redText "x" $$ showPatch x $$
                                       redText ":> y" $$ showPatch y $$
                                       redText "y'" $$ showPatch y' $$
                                       redText ":> x'" $$ showPatch x'
