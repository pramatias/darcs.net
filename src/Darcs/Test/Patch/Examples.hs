--  Copyright (C) 2002-2005,2007 David Roundy
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2, or (at your option)
--  any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; see the file COPYING.  If not, write to
--  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
--  Boston, MA 02110-1301, USA.

{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-deprecations #-}
{-# LANGUAGE CPP #-}

module Darcs.Test.Patch.Examples ( testInfo, patchExampleTests ) where

import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString.Char8 as BC ( pack )
import qualified Data.ByteString as B ( empty )
import Darcs.Patch
     ( Patchy, commute, invert, merge, effect
     , Named, namepatch
     , readPatch, showPatch
     , fromPrim, canonize, sortCoalesceFL
     , adddir, addfile, hunk, binary, rmdir, rmfile, tokreplace )
import Darcs.Patch.Prim ( PrimOf, FromPrim )
import Darcs.Patch.Prim.V1 ( Prim )
import qualified Darcs.Patch.V1 as V1 ( Patch )
import Darcs.Test.Patch.Test
    ( checkAPatch )
import Darcs.Test.Patch.Utils ( testStringList )
import Printer ( renderPS )
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Show
import Darcs.Witnesses.Sealed ( Sealed(Sealed), unsafeUnseal )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Test.Framework ( Test )

#include "gadts.h"
#include "impossible.h"

type Patch = V1.Patch Prim

testInfo :: String
testInfo = unlines
  [ "There are a total of "++ show (length primitiveTestPatches) ++" primitive patches."
  , "There are a total of "++ show (length testPatches) ++" patches."
  ]

patchExampleTests :: [Test]
patchExampleTests =
        [
         testStringList "Checking known commutes" commuteTests,
         testStringList "Checking known merges" mergeTests,
         testStringList "Checking known canons" canonizationTests,
         testStringList "Checking merge swaps" mergeSwapTests,
         testStringList "Checking that the patch validation works" testCheck,
         testStringList "Checking commute/recommute" commuteRecommuteTests,
         testStringList "Checking merge properties" genericMergeTests,
         testStringList "Checking primitive patch IO functions" primitiveShowReadTests,
         testStringList "Checking IO functions" showReadTests,
         testStringList "Checking primitive commute/recommute" primitiveCommuteRecommuteTests
        ]

-- The unit tester function is really just a glorified map for functions that
-- return lists, in which the lists get concatenated (where map would end up
-- with a list of lists).

quickmerge :: (FL Patch :\/: FL Patch) C(x y) -> FL Patch C(y z)
quickmerge (p1:\/:p2) = case merge (p1:\/:p2) of
                        _ :/\: p1' -> unsafeCoercePEnd p1'

type PatchUnitTest p = FORALL(x y) p C(x y) -> [String]
type ParallelPatchUnitTest = FORALL(x y z) FL Patch C(x y) -> FL Patch C(x z) -> [String]
type SerialPatchUnitTest = FORALL(x y z) FL Patch C(y z) -> FL Patch C(x y) -> [String]

parallelPairUnitTester :: ParallelPatchUnitTest -> [(FL Patch:\/:FL Patch) C(x y)] -> [String]
parallelPairUnitTester _ []        = []
parallelPairUnitTester thetest ((p1:\/:p2):ps)
    = (thetest p1 p2)++(parallelPairUnitTester thetest ps)

pairUnitTester :: SerialPatchUnitTest -> [(FL Patch:<FL Patch) C(x y)] -> [String]
pairUnitTester _ []        = []
pairUnitTester thetest ((p1:<p2):ps)
    = (thetest p1 p2)++(pairUnitTester thetest ps)


-- ----------------------------------------------------------------------
-- * Show/Read tests
-- ----------------------------------------------------------------------

-- | This test involves calling 'show' to print a string describing a patch,
--   and then using readPatch to read it back in, and making sure the patch we
--   read in is the same as the original.  Useful for making sure that I don't
--   have any stupid IO bugs.
showReadTests :: [String]
showReadTests = concatMap (tShowRead eqFLUnsafe) testPatches ++
                  concatMap (tShowRead unsafeCompare) testPatchesNamed

primitiveShowReadTests :: [String]
primitiveShowReadTests = concatMap (tShowRead eqFLUnsafe) primitiveTestPatches

tShowRead :: (Show2 p, Patchy p) => (FORALL(x y w z) p C(x y) -> p C(w z) -> Bool) -> PatchUnitTest p
tShowRead eq p =
    case readPatch $ renderPS $ showPatch p of
    Just (Sealed p') -> if p' `eq` p then []
                        else ["Failed to read shown:  "++(show2 p)++"\n"]
    Nothing -> ["Failed to read at all:  "++(show2 p)++"\n"]

-- ----------------------------------------------------------------------
-- * Canonization tests
-- ----------------------------------------------------------------------

-- | This is a set of known correct canonizations, to make sure that I'm
--   canonizing as I ought.
canonizationTests :: [String]
canonizationTests = concatMap checkKnownCanon knownCanons
checkKnownCanon :: FORALL(x y) (FL Patch C(x y), FL Patch C(x y)) -> [String]
checkKnownCanon (p1,p2) =
    if isIsEq $ eqFL (mapFL_FL fromPrim $ concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect p1) p2
    then []
    else ["Canonization failed:\n"++show p1++"canonized is\n"
          ++show (mapFL_FL fromPrim $ concatFL $ mapFL_FL canonize $ sortCoalesceFL $ effect p1 :: FL Patch C(x y))
          ++"which is not\n"++show p2]
knownCanons :: [(FL Patch C(x y),FL Patch C(x y))]
knownCanons =
    [(quickhunk 1 "abcde" "ab" :>: NilFL, quickhunk 3 "cde"   "" :>: NilFL),
     (quickhunk 1 "abcde" "bd" :>: NilFL,
      quickhunk 1 "a" "" :>:
      quickhunk 2 "c" "" :>:
      quickhunk 3 "e" "" :>: NilFL),
     (quickhunk 4 "a" "b" :>:
      quickhunk 1 "c" "d" :>: NilFL,
      quickhunk 1 "c" "d" :>:
      quickhunk 4 "a" "b" :>: NilFL),
     (quickhunk 1 "a" "" :>:
      quickhunk 1 "" "b" :>: NilFL,
      quickhunk 1 "a" "b" :>: NilFL),
     (quickhunk 1 "ab" "c" :>:
      quickhunk 1 "cd" "e" :>: NilFL,
      quickhunk 1 "abd" "e" :>: NilFL),
     (quickhunk 1 "abcde" "cde" :>: NilFL, quickhunk 1 "ab" "" :>: NilFL),
     (quickhunk 1 "abcde" "acde" :>: NilFL, quickhunk 2 "b" "" :>: NilFL)]

quickhunk :: (FromPrim p, PrimOf p ~ Prim) => Int -> String -> String -> p C(x y)
quickhunk l o n = fromPrim $ hunk "test" l (map (\c -> BC.pack [c]) o)
                                             (map (\c -> BC.pack [c]) n)

-- ----------------------------------------------------------------------
-- * Merge/unmgerge tests
-- ----------------------------------------------------------------------

-- | It should always be true that if two patches can be unmerged, then merging
--   the resulting patches should give them back again.
genericMergeTests :: [String]
genericMergeTests =
  case take 400 [(p1:\/:p2)|
                 i <- [0..(length testPatches)-1],
                 p1<-[testPatches!!i],
                 p2<-drop i testPatches,
                 checkAPatch (invert p2 :>: p1 :>: NilFL)] of
  merge_pairs -> (parallelPairUnitTester tMergeEitherWayValid merge_pairs) ++
                 (parallelPairUnitTester tMergeSwapMerge merge_pairs)
tMergeEitherWayValid   :: ParallelPatchUnitTest
tMergeEitherWayValid p1 p2 =
  case p2 :>: quickmerge (p1:\/: p2) :>: NilFL of
  combo2 ->
    case p1 :>: quickmerge (p2:\/: p1) :>: NilFL of
    combo1 ->
      if not $ checkAPatch combo1
      then ["oh my combo1 invalid:\n"++show p1++"and...\n"++show p2++show combo1]
      else
        if checkAPatch (invert combo1 :>: combo2 :>: NilFL)
        then []
        else ["merge both ways invalid:\n"++show p1++"and...\n"++show p2++
              show combo1++
              show combo2]
tMergeSwapMerge   :: ParallelPatchUnitTest
tMergeSwapMerge p1 p2 =
  if merge (p2 :\/: p1) `eqSwapped` merge (p1 :\/: p2)
  then []
  else ["Failed to swap merges:\n"++show p1++"and...\n"++show p2
        ++"merged:\n"++show (merge (p1:\/:p2))++"\n"
        ++"merged swapped:\n"++show (merge (p2:\/: p1))++"\n"]
    where eqSwapped :: (FL Patch :/\: FL Patch) C(x y) -> (FL Patch :/\: FL Patch) C(y x) -> Bool
          eqSwapped (x1 :/\: y1) (y2 :/\: x2) | IsEq <- eqFL x1 x2, IsEq <- eqFL y1 y2 = True
          eqSwapped _ _ = False

instance Show2 p => Show ((p :/\: p) C(x y)) where
   show (x :/\: y) = show2 x ++ " :/\\: " ++ show2 y
instance MyEq p => Eq ((p :/\: p) C(x y)) where
   (x :/\: y) == (x' :/\: y') = isIsEq (x =\/= x') && isIsEq (y =\/= y')

-- ----------------------------------------------------------------------
-- * Commute/recommute tests
-- ----------------------------------------------------------------------

-- | Here we test to see if commuting patch A and patch B and then commuting
-- the result gives us patch A and patch B again.  The set of patches (A,B)
-- is chosen from the set of all pairs of test patches by selecting those which
-- commute with one another.
commuteRecommuteTests :: [String]
commuteRecommuteTests =
  case take 200 [(p2:<p1)|
                 p1<-testPatches,
                 p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatches,
                 commute (p1:>p2) /= Nothing] of
  commute_pairs -> pairUnitTester tCommuteRecommute commute_pairs
primitiveCommuteRecommuteTests :: [String]
primitiveCommuteRecommuteTests =
  pairUnitTester tCommuteRecommute
    [(p1:<p2)|
     p1<-primitiveTestPatches,
     p2<-primitiveTestPatches,
     commute (p2:>p1) /= Nothing,
     checkAPatch (p2:>:p1:>:NilFL)]
tCommuteRecommute   :: SerialPatchUnitTest
tCommuteRecommute p1 p2 =
    if (commute (p2:>p1) >>= commute) == Just (p2:>p1)
       then []
       else ["Failed to recommute:\n"++(show p2)++(show p1)++
            "we saw it as:\n"++show (commute (p2:>p1))++
             "\nAnd recommute was:\n"++show (commute (p2:>p1) >>= commute)
             ++ "\n"]

-- ----------------------------------------------------------------------
-- * Commute tests
-- ----------------------------------------------------------------------

-- | Here we provide a set of known interesting commutes.
commuteTests :: [String]
commuteTests =
    concatMap checkKnownCommute knownCommutes++
    concatMap checkCantCommute knownCantCommute
checkKnownCommute :: ((FL Patch:< FL Patch) C(x y), (FL Patch:< FL Patch) C(x y)) -> [String]
checkKnownCommute (p1:<p2,p2':<p1') =
   case commute (p2:>p1) of
   Just (p1a:>p2a) ->
       if (p2a:< p1a) == (p2':< p1')
       then []
       else ["Commute gave wrong value!\n"++show p1++"\n"++show p2
             ++"should be\n"++show p2'++"\n"++show p1'
             ++"but is\n"++show p2a++"\n"++show p1a]
   Nothing -> ["Commute failed!\n"++show p1++"\n"++show p2]
   ++
   case commute (p1':>p2') of
   Just (p2a:>p1a) ->
       if (p1a:< p2a) == (p1:< p2)
       then []
       else ["Commute gave wrong value!\n"++show p2a++"\n"++show p1a
             ++"should have been\n"++show p2'++"\n"++show p1']
   Nothing -> ["Commute failed!\n"++show p2'++"\n"++show p1']
knownCommutes :: [((FL Patch:<FL Patch) C(x y),(FL Patch:<FL Patch) C(x y))]
knownCommutes = [
                  (testhunk 1 [] ["A"]:<
                   testhunk 2 [] ["B"],
                   testhunk 3 [] ["B"]:<
                   testhunk 1 [] ["A"]),
                  (fromPrim (tokreplace "test" "A-Za-z_" "old" "new"):<
                   testhunk 2
                   ["hello world all that is old is good old_"]
                   ["I don't like old things"],
                   testhunk 2
                   ["hello world all that is new is good old_"]
                   ["I don't like new things"]:<
                   fromPrim (tokreplace "test" "A-Za-z_" "old" "new")),
                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 2 ["C"] ["D"],
                   testhunk 2 ["C"] ["D"]:<
                   testhunk 1 ["A"] ["B"]),
                  (fromPrim (rmfile "NwNSO"):<
                   (quickmerge (fromPrim (addfile "hello"):\/:fromPrim (addfile "hello"))),
                   (quickmerge (fromPrim (addfile "hello"):\/:fromPrim (addfile "hello"))):<
                   fromPrim (rmfile "NwNSO")),

                  (quickmerge (testhunk 3 ["o"] ["n"]:\/:
                               testhunk 3 ["o"] ["v"]):<
                   testhunk 1 [] ["a"],
                   testhunk 1 [] ["a"]:<
                   quickmerge (testhunk 2 ["o"] ["n"]:\/:
                               testhunk 2 ["o"] ["v"])),

                  (testhunk 1 ["A"] []:<
                   testhunk 3 ["B"] [],
                   testhunk 2 ["B"] []:<
                   testhunk 1 ["A"] []),

                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 2 ["B"] ["C"],
                   testhunk 2 ["B"] ["C"]:<
                   testhunk 1 ["A"] ["B"]),

                  (testhunk 1 ["A"] ["B"]:<
                   testhunk 3 ["B"] ["C"],
                   testhunk 3 ["B"] ["C"]:<
                   testhunk 1 ["A"] ["B"]),

                  (testhunk 1 ["A"] ["B","C"]:<
                   testhunk 2 ["B"] ["C","D"],
                   testhunk 3 ["B"] ["C","D"]:<
                   testhunk 1 ["A"] ["B","C"])]
  where testhunk l o n = fromPrim $ hunk "test" l (map BC.pack o) (map BC.pack n)

checkCantCommute :: (FL Patch:< FL Patch) C(x y) -> [String]
checkCantCommute (p1:<p2) =
    case commute (p2:>p1) of
    Nothing -> []
    _ -> [show p1 ++ "\n\n" ++ show p2 ++
          "\nArgh, these guys shouldn't commute!\n"]
knownCantCommute :: [(FL Patch:< FL Patch) C(x y)]
knownCantCommute = [
                      (testhunk 2 ["o"] ["n"]:<
                       testhunk 1 [] ["A"]),
                      (testhunk 1 [] ["A"]:<
                       testhunk 1 ["o"] ["n"]),
                      (quickmerge (testhunk 2 ["o"] ["n"]:\/:
                                   testhunk 2 ["o"] ["v"]):<
                       testhunk 1 [] ["a"]),
                      (fromPrim (hunk "test" 1 ([BC.pack "a"]) ([BC.pack "b"])):<
                       fromPrim (addfile "test"))]
  where testhunk l o n = fromPrim $ hunk "test" l (map BC.pack o) (map BC.pack n)

-- ----------------------------------------------------------------------
-- * Merge tests
-- ----------------------------------------------------------------------

-- | Here we provide a set of known interesting merges.
mergeTests :: [String]
mergeTests =
    concatMap checkKnownMergeEquiv knownMergeEquivs++
    concatMap checkKnownMerge knownMerges
checkKnownMerge :: ((FL Patch:\/: FL Patch) C(x y), FL Patch C(y z)) -> [String]
checkKnownMerge (p1:\/:p2,p1') =
   case merge (p1:\/:p2) of
   _ :/\: p1a ->
       if isIsEq (p1a `eqFL` p1')
       then []
       else ["Merge gave wrong value!\n"++show p1++show p2
             ++"I expected\n"++show p1'
             ++"but found instead\n"++show p1a]
knownMerges :: [((FL Patch:\/:FL Patch) C(x y),FL Patch C(y z))]
knownMerges = [
                (testhunk 2 [BC.pack "c"] [BC.pack "d",BC.pack "e"]:\/:
                 testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"],
                 testhunk 3 [BC.pack "c"] [BC.pack "d",BC.pack "e"]),
                (testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"]:\/:
                 testhunk 2 [BC.pack "c"] [BC.pack "d",BC.pack "e"],
                 testhunk 1 [BC.pack "x"] [BC.pack "a",BC.pack "b"]),
                (testhunk 3 [BC.pack "A"] []:\/:
                 testhunk 1 [BC.pack "B"] [],
                 testhunk 2 [BC.pack "A"] []),
                (fromPrim (rmdir "./test/world"):\/:
                 fromPrim (hunk "./world" 3 [BC.pack "A"] []),
                 fromPrim (rmdir "./test/world")),

                ((quickhunk 1 "a" "bc" :>:
                  quickhunk 6 "d" "ef" :>: NilFL):\/:
                 (quickhunk 3 "a" "bc" :>:
                  quickhunk 8 "d" "ef" :>: NilFL),
                 (quickhunk 1 "a" "bc" :>:
                  quickhunk 7 "d" "ef" :>: NilFL)),

                (testhunk 1 [BC.pack "A"] [BC.pack "B"]:\/:
                 testhunk 2 [BC.pack "B"] [BC.pack "C"],
                 testhunk 1 [BC.pack "A"] [BC.pack "B"]),

                (testhunk 2 [BC.pack "A"] [BC.pack "B",BC.pack "C"]:\/:
                 testhunk 1 [BC.pack "B"] [BC.pack "C",BC.pack "D"],
                 testhunk 3 [BC.pack "A"] [BC.pack "B",BC.pack "C"])]
  where testhunk l o n = fromPrim $ hunk "test" l o n
checkKnownMergeEquiv :: ((FL Patch:\/:FL Patch) C(x y),FL Patch C(y z)) -> [String]
checkKnownMergeEquiv (p1:\/: p2, pe) =
    case quickmerge (p1:\/:p2) of
    p1' -> if checkAPatch (invert p1 :>: p2 :>: p1' :>: invert pe :>: NilFL)
           then []
           else ["Oh no, merger isn't equivalent...\n"++show p1++"\n"++show p2
                 ++"in other words\n" ++ show (p1 :\/: p2)
                 ++"merges as\n" ++ show (merge $ p1 :\/: p2)
                 ++"merges to\n" ++ show (quickmerge $ p1 :\/: p2)
                 ++"which is equivalent to\n" ++ show (effect p1')
                 ++ "should all work out to\n"
                 ++ show pe]
knownMergeEquivs :: [((FL Patch :\/: FL Patch) C(x y), FL Patch C(y z))]
knownMergeEquivs = [

                     -- The following tests are going to be failed by the
                     -- Conflictor code as a cleanup.

                     --(addfile "test":\/:
                     -- adddir "test",
                     -- joinPatches (adddir "test" :>:
                     --              addfile "test-conflict" :>: NilFL)),
                     --(move "silly" "test":\/:
                     -- adddir "test",
                     -- joinPatches (adddir "test" :>:
                     --              move "silly" "test-conflict" :>: NilFL)),
                     --(addfile "test":\/:
                     -- move "old" "test",
                     -- joinPatches (addfile "test" :>:
                     --              move "old" "test-conflict" :>: NilFL)),
                     --(move "a" "test":\/:
                     -- move "old" "test",
                     -- joinPatches (move "a" "test" :>:
                     --              move "old" "test-conflict" :>: NilFL)),
                     (fromPrim (hunk "test" 1 [] [BC.pack "A"]) :\/:
                       fromPrim (hunk "test" 1 [] [BC.pack "B"]),
                       fromPrim (hunk "test" 1 [] [BC.pack "A", BC.pack "B"])),
                     (fromPrim (hunk "test" 1 [] [BC.pack "a"]):\/:
                      fromPrim (hunk "test" 1 [BC.pack "b"] []),
                      unsafeCoerceP NilFL),
                      --hunk "test" 1 [] [BC.pack "v v v v v v v",
                      --                  BC.pack "*************",
                      --                  BC.pack "a",
                      --                  BC.pack "b",
                      --                  BC.pack "^ ^ ^ ^ ^ ^ ^"]),
                     (quickhunk 4 "a"  "" :\/:
                      quickhunk 3 "a"  "",
                      quickhunk 3 "aa" ""),
                     ((quickhunk 1 "a" "bc" :>:
                       quickhunk 6 "d" "ef" :>: NilFL) :\/:
                       (quickhunk 3 "a" "bc" :>:
                        quickhunk 8 "d" "ef" :>: NilFL),
                      quickhunk 3 "a" "bc" :>:
                      quickhunk 8 "d" "ef" :>:
                      quickhunk 1 "a" "bc" :>:
                      quickhunk 7 "d" "ef" :>: NilFL),
                     (quickmerge (quickhunk 2 "" "bd":\/:quickhunk 2 "" "a") :\/:
                              quickmerge (quickhunk 2 "" "c":\/:quickhunk 2 "" "a"),
                              quickhunk 2 "" "abdc")
                     ]


-- | It also is useful to verify that it doesn't matter which order we specify
--   the patches when we merge.
mergeSwapTests :: [String]
mergeSwapTests =
    concat
              [checkMergeSwap p1 p2 |
               p1<-primitiveTestPatches,
               p2<-primitiveTestPatches,
               checkAPatch (invert p1:>:p2:>:NilFL)
              ]
checkMergeSwap :: FL Patch C(x y) -> FL Patch C(x z) -> [String]
checkMergeSwap p1 p2 =
    case merge (p2:\/:p1) of
    _ :/\: p2' ->
        case merge (p1:\/:p2) of
        _ :/\: p1' ->
            case commute (p1:>p2') of
            Just (_:>p1'b) ->
                if not $ p1'b `eqFLUnsafe` p1'
                then ["Merge swapping problem with...\np1 "++
                      show p1++"merged with\np2 "++
                      show p2++"p1' is\np1' "++
                      show p1'++"p1'b is\np1'b  "++
                      show p1'b
                     ]
                else []
            Nothing -> ["Merge commuting problem with...\np1 "++
                        show p1++"merged with\np2 "++
                        show p2++"gives\np2' "++
                        show p2'++"which doesn't commute with p1.\n"
                       ]

-- ----------------------------------------------------------------------
-- Patch test data
-- This is where we define the set of patches which we run our tests on.  This
-- should be kept up to date with as many interesting permutations of patch
-- types as possible.
-- ----------------------------------------------------------------------

testPatches :: [FL Patch C(x y)]
testPatchesNamed :: [Named Patch C(x y)]
testPatchesAddfile :: [FL Patch C(x y)]
testPatchesRmfile :: [FL Patch C(x y)]
testPatchesHunk :: [FL Patch C(x y)]
primitiveTestPatches :: [FL Patch C(x y)]
testPatchesBinary :: [FL Patch C(x y)]
testPatchesCompositeNocom :: [FL Patch C(x y)]
testPatchesComposite :: [FL Patch C(x y)]
testPatchesTwoCompositeHunks :: [FL Patch C(x y)]
testPatchesCompositeHunks :: [FL Patch C(x y)]
testPatchesCompositeFourHunks :: [FL Patch C(x y)]
testPatchesMerged :: [FL Patch C(x y)]
validPatches :: [FL Patch C(x y)]

testPatchesNamed = [unsafePerformIO $
                      namepatch "date is" "patch name" "David Roundy" []
                                (fromPrim $ addfile "test"),
                      unsafePerformIO $
                      namepatch "Sat Oct 19 08:31:13 EDT 2002"
                                "This is another patch" "David Roundy"
                                ["This log file has","two lines in it"]
                                (fromPrim $ rmfile "test")]
testPatchesAddfile = map fromPrim
                       [addfile "test",adddir "test",addfile "test/test"]
testPatchesRmfile = map invert testPatchesAddfile
testPatchesHunk  =
    [fromPrim (hunk file line old new) |
     file <- ["test"],
     line <- [1,2],
     old <- map (map BC.pack) partials,
     new <- map (map BC.pack) partials,
     old /= new
    ]
    where partials  = [["A"],["B"],[],["B","B2"]]

primitiveTestPatches = testPatchesAddfile ++
                         testPatchesRmfile ++
                         testPatchesHunk ++
                         [unsafeUnseal.fromJust.readPatch $
                          BC.pack "move ./test/test ./hello",
                          unsafeUnseal.fromJust.readPatch $
                          BC.pack "move ./test ./hello"] ++
                         testPatchesBinary

testPatchesBinary =
    [fromPrim $ binary "./hello"
     (BC.pack $ "agadshhdhdsa75745457574asdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg")
     (BC.pack $ "adafjttkykrehhtrththrthrthre" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaasdgg" ++
      "a326424677373735753246463gadshhdhdsaagg"),
     fromPrim $ binary "./hello"
     B.empty
     (BC.pack "adafjttkykrere")]

testPatchesCompositeNocom =
    take 50 [p1+>+p2|
             p1<-primitiveTestPatches,
             p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) primitiveTestPatches,
             commute (p1:>p2) == Nothing]

testPatchesComposite =
    take 100 [p1+>+p2|
              p1<-primitiveTestPatches,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) primitiveTestPatches,
              commute (p1:>p2) /= Nothing,
              commute (p1:>p2) /= Just (unsafeCoerceP p2:>unsafeCoerceP p1)]

testPatchesTwoCompositeHunks =
    take 100 [p1+>+p2|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk]

testPatchesCompositeHunks =
    take 100 [p1+>+p2+>+p3|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk,
              p3<-filter (\p->checkAPatch (p1:>:p2:>:p:>:NilFL)) testPatchesHunk]

testPatchesCompositeFourHunks =
    take 100 [p1+>+p2+>+p3+>+p4|
              p1<-testPatchesHunk,
              p2<-filter (\p->checkAPatch (p1:>:p:>:NilFL)) testPatchesHunk,
              p3<-filter (\p->checkAPatch (p1:>:p2:>:p:>:NilFL)) testPatchesHunk,
              p4<-filter (\p->checkAPatch (p1:>:p2:>:p3:>:p:>:NilFL)) testPatchesHunk]

testPatchesMerged =
  take 200
    [p2+>+quickmerge (p1:\/:p2) |
     p1<-take 10 (drop 15 testPatchesCompositeHunks)++primitiveTestPatches
         ++take 10 (drop 15 testPatchesTwoCompositeHunks)
         ++ take 2 (drop 4 testPatchesCompositeFourHunks),
     p2<-take 10 testPatchesCompositeHunks++primitiveTestPatches
         ++take 10 testPatchesTwoCompositeHunks
         ++take 2 testPatchesCompositeFourHunks,
     checkAPatch (invert p1 :>: p2 :>: NilFL),
     commute (p2:>p1) /= Just (p1:>p2)
    ]

testPatches =  primitiveTestPatches ++
                testPatchesComposite ++
                testPatchesCompositeNocom ++
                testPatchesMerged

-- ----------------------------------------------------------------------
-- * Check patch test
-- ----------------------------------------------------------------------

validPatches = [(quickhunk 4 "a" "b" :>:
                 quickhunk 1 "c" "d" :>: NilFL),
                (quickhunk 1 "a" "bc" :>:
                 quickhunk 1 "b" "d" :>: NilFL),
                (quickhunk 1 "a" "b" :>:
                 quickhunk 1 "b" "d" :>: NilFL)]++testPatches

testCheck :: [String]
testCheck = concatMap tTestCheck validPatches
tTestCheck :: PatchUnitTest (FL Patch)
tTestCheck p = if checkAPatch p
                 then []
                 else ["Failed the check:  "++show p++"\n"]

