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

-- This module was split out from Darcs.Test.Patch.Unit to assist with
-- adding witnesses.
module Darcs.Test.Patch.Unit2 ( patchUnitTests2, notDuplicatestriple ) where

import Darcs.Patch.Prim ( PrimPatch, PrimOf, join )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Patchy, Commute(..) )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.V2.Real ( isConsistent, isDuplicate )
import Darcs.Test.Patch.Properties
     ( recommute, commuteInverses, permutivity, partialPermutivity
     , mergeEitherWay, show_read
     , joinCommute
     )
import Darcs.Test.Patch.QuickCheck
     ( Tree, TreeWithFlattenPos
     , RepoModel, WithStartState
     , propIsMergeable, propConsistentTreeFlattenings
     )
import qualified Darcs.Test.Patch.QuickCheck as T
     ( commuteTripleFromTree, commutePairFromTree, commutePairFromTWFP
     , mergePairFromTree, mergePairFromTWFP
     , patchFromTree
     )
import Darcs.Test.Patch.Test () -- for instance Eq Patch
import Darcs.Test.Patch.Utils ( testConditional )
import Darcs.Witnesses.Eq ( unsafeCompare )
import Darcs.Witnesses.Ordered ( (:>)(..), (:/\:)(..), (:\/:)(..), FL )
import Darcs.Witnesses.Sealed ( Sealed, unseal, unseal2 )
import Printer ( Doc )

import Data.Maybe ( isNothing )
import Test.Framework ( Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )

patchFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) p C(y z) -> t) -> WithStartState RepoModel (Tree Prim) C(x) -> t
patchFromTree = T.patchFromTree

mergePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :\/: p) C(y z) -> t) -> WithStartState RepoModel (Tree Prim) C(x) -> t
mergePairFromTree = T.mergePairFromTree

mergePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :\/: p) C(y z) -> t) -> WithStartState RepoModel (TreeWithFlattenPos Prim) C(x) -> t
mergePairFromTWFP = T.mergePairFromTWFP

commutePairFromTWFP :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p) C(y z) -> t) -> WithStartState RepoModel (TreeWithFlattenPos Prim) C(x) -> t
commutePairFromTWFP = T.commutePairFromTWFP

commutePairFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p) C(y z) -> t) -> WithStartState RepoModel (Tree Prim) C(x) -> t
commutePairFromTree = T.commutePairFromTree

commuteTripleFromTree :: (RepoPatch p, PrimOf p ~ Prim) => (FORALL(y z) (p :> p :> p) C(y z) -> t) -> WithStartState RepoModel (Tree Prim) C(x) -> t
commuteTripleFromTree = T.commuteTripleFromTree


patchUnitTests2 :: [Test]
patchUnitTests2 = [--do putStr "Checking with quickcheck that real patches have consistent flattenings... "
                    --   quickCheck (not . isBottomTimeOut (Just 10) . propConsistentTreeFlattenings) >> return 0
                    -- The following fails because of setpref patches...
                    --,do putStr "Checking prim inverse doesn't commute using QuickCheck... "
                    --    simpleCheck (inverseDoesntCommute :: Prim -> Maybe Doc)
                    testProperty "Checking prim join commute using QuickCheck... " (unseal2 (isNothing . joinCommute join)),
                    --,do putStr "Checking prim recommute using QuickCheck... "
                    --    simpleCheck (recommute
                    --                 (commute :: Prim :> Prim
                    --                          -> Maybe (Prim :> Prim)))
                    testProperty "Checking that readPatch and showPatch work on RealPatch... "
                                 (isNothing . (unseal $ patchFromTree $ (show_read :: RealPatch Prim C(x y) -> Maybe Doc))),
                    testProperty "Checking that readPatch and showPatch work on FL RealPatch... "
                                 (isNothing . (unseal2 $ (show_read :: FL (RealPatch Prim) C(x y) -> Maybe Doc))),
                    testProperty "Checking that tree flattenings are consistent... " propConsistentTreeFlattenings,
                    testProperty "Checking with quickcheck that real patches are consistent... "
                                 (isNothing . (unseal $ patchFromTree $ isConsistent)),

                    testProperty "Checking we can do merges using QuickCheck"
                                 (isNothing . (propIsMergeable ::
                                                Sealed (WithStartState RepoModel (Tree Prim))
                                                -> Maybe (Tree (RealPatch Prim) C(x)))),
                    testProperty "Checking recommute using QuickCheck Tree generator"
                                 (isNothing. (unseal $ commutePairFromTree $
                                              (recommute commuteReals))),
                    testProperty "Checking recommute using QuickCheck TWFP generator"
                                 (isNothing . (unseal $ commutePairFromTWFP $
                                               (recommute commuteReals))),
                    testConditional "Checking nontrivial recommute"
                                    (unseal $ commutePairFromTree $ nontrivialReals)
                                    (unseal $ commutePairFromTree $
                                     (recommute commuteReals)),
                    testConditional "Checking nontrivial recommute using TWFP"
                                    (unseal $ commutePairFromTWFP $ nontrivialReals)
                                    (unseal $ commutePairFromTWFP $
                                      (recommute commuteReals)),

                    testProperty "Checking inverses commute using QuickCheck Tree generator"
                                 (isNothing . (unseal $ commutePairFromTree $
                                               (commuteInverses commuteReals))),
                    testProperty "Checking inverses commute using QuickCheck TWFP generator"
                                 (isNothing . (unseal $ commutePairFromTWFP $
                                               (commuteInverses commuteReals))),
                    testConditional "Checking nontrivial inverses commute"
                                    (unseal $ commutePairFromTree $ nontrivialReals)
                                    (unseal $ commutePairFromTree $
                                     (commuteInverses commuteReals)),
                    testConditional "Checking nontrivial inverses commute using TWFP"
                                    (unseal $ commutePairFromTWFP $ nontrivialReals)
                                    (unseal $ commutePairFromTWFP $
                                     (commuteInverses commuteReals)),

                    testProperty "Checking merge either way using QuickCheck TWFP generator"
                                 (isNothing . (unseal $ mergePairFromTWFP $ mergeEitherWayReals)),
                    testProperty "Checking merge either way using QuickCheck Tree generator"
                                 (isNothing . (unseal $ mergePairFromTree $ mergeEitherWayReals)),
                    testConditional "Checking nontrivial merge either way"
                                    (unseal $ mergePairFromTree $ nontrivialMergeReals)
                                    (unseal $ mergePairFromTree $ mergeEitherWayReals),
                    testConditional "Checking nontrivial merge either way using TWFP"
                                    (unseal $ mergePairFromTWFP $ nontrivialMergeReals)
                                    (unseal $ mergePairFromTWFP $ mergeEitherWayReals),

                    testConditional "Checking permutivity"
                                    (unseal $ commuteTripleFromTree notDuplicatestriple)
                                    (unseal $ commuteTripleFromTree $ permutivity commuteReals),
                    testConditional "Checking partial permutivity"
                                    (unseal $ commuteTripleFromTree notDuplicatestriple)
                                    (unseal $ commuteTripleFromTree $ partialPermutivity commuteReals),
                    testConditional "Checking nontrivial permutivity"
                                    (unseal $ commuteTripleFromTree
                                               (\t -> nontrivialTriple t && notDuplicatestriple t))
                                    (unseal $ commuteTripleFromTree $
                                      (permutivity commuteReals))
                   ]

notDuplicatestriple :: (RealPatch prim :> RealPatch prim :> RealPatch prim) C(x y) -> Bool
notDuplicatestriple (a :> b :> c) = not (isDuplicate a || isDuplicate b || isDuplicate c)

--not_duplicates_pair :: RealPatch prim :> RealPatch prim -> Bool
--not_duplicates_pair (a :> b) = not $ any isDuplicate [a,b]

nontrivialTriple :: PrimPatch prim => (RealPatch prim :> RealPatch prim :> RealPatch prim) C(x y) -> Bool
nontrivialTriple (a :> b :> c) =
    case commute (a :> b) of
    Nothing -> False
    Just (b' :> a') ->
      case commute (a' :> c) of
      Nothing -> False
      Just (c'' :> a'') ->
        case commute (b :> c) of
        Nothing -> False
        Just (c' :> b'') -> (not (a `unsafeCompare` a') || not (b `unsafeCompare` b')) &&
                            (not (c' `unsafeCompare` c) || not (b'' `unsafeCompare` b)) &&
                            (not (c'' `unsafeCompare` c) || not (a'' `unsafeCompare` a'))

commuteReals :: PrimPatch prim => (RealPatch prim :> RealPatch prim) C(x y) -> Maybe ((RealPatch prim :> RealPatch prim) C(x y))
commuteReals = commute

mergeEitherWayReals :: PrimPatch prim => (RealPatch prim :\/: RealPatch prim) C(x y) -> Maybe Doc
mergeEitherWayReals = mergeEitherWay

nontrivialReals :: PrimPatch prim => (RealPatch prim :> RealPatch prim) C(x y) -> Bool
nontrivialReals = nontrivialCommute

nontrivialCommute :: Patchy p => (p :> p) C(x y) -> Bool
nontrivialCommute (x :> y) = case commute (x :> y) of
                              Just (y' :> x') -> not (y' `unsafeCompare` y) ||
                                                 not (x' `unsafeCompare` x)
                              Nothing -> False

nontrivialMergeReals :: PrimPatch prim => (RealPatch prim :\/: RealPatch prim) C(x y) -> Bool
nontrivialMergeReals = nontrivialMerge

nontrivialMerge :: (Patchy p, Merge p) => (p :\/: p) C(x y) -> Bool
nontrivialMerge (x :\/: y) = case merge (x :\/: y) of
                              y' :/\: x' -> not (y' `unsafeCompare` y) ||
                                            not (x' `unsafeCompare` x)

