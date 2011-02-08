-- Copyright (C) 2003-2004 David Roundy
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

{-# LANGUAGE CPP , ScopedTypeVariables #-}

#include "gadts.h"

module Darcs.Patch.Depends ( getTagsRight,
                             areUnrelatedRepos,
                             mergeThem, findCommonWithThem,
                             countUsThem, removeFromPatchSet,
                 optimizePatchset, deepOptimizePatchset,
                 slightlyOptimizePatchset,
                 getPatchesBeyondTag, getPatchesInTag,
                 splitOnTag,
                 newsetUnion, newsetIntersection,
                 commuteToEnd, findUncommon, merge2FL
               ) where
import Data.List ( delete, intersect, (\\) )

import Darcs.Patch ( RepoPatch, getdeps, commute, commuteFLorComplain, commuteRL )
import Darcs.Patch.Info ( PatchInfo, isTag, humanFriendly )
import Darcs.Patch.Merge ( mergeFL )
import Darcs.Patch.Permutations ( partitionFL, partitionRL, removeSubsequenceRL )
import Darcs.Patch.PatchInfoAnd( PatchInfoAnd, hopefully, hopefullyM, info )
import Darcs.Witnesses.Eq ( EqCheck(..), (=\/=), (=/\=) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Witnesses.Ordered ( (:\/:)(..), (:/\:)(..), (:>)(..), (:>)(..),
                                 (+>+), mapFL,
                                 RL(..), FL(..), isShorterThanRL, (+<+),
                                 reverseFL, reverseRL, mapRL, )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, newset2RL )
import Darcs.ProgressPatches ( progressRL )
import Darcs.Witnesses.Sealed (Sealed(..), FlippedSeal(..), flipSeal, seal )
import Printer ( renderString, vcat )
#include "impossible.h"

{-|
with_partial_intersection takes two 'PatchSet's and splits them into a /common/
intersection portion and two sets of patches.  The intersection, however,
is only lazily determined, so there is no guarantee that all intersecting
patches will be included in the intersection 'PatchSet'.  This is a pretty
efficient function, because it makes use of the already-broken-up nature of
'PatchSet's.

Note that the first argument to with_partial_intersection should be
the repository that is more cheaply accessed (i.e. local), as
with_partial_intersection does its best to reduce the number of
inventories that are accessed from its rightmost argument.
-}

with_partial_intersection
    :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y)
    -> (FORALL(a c) RL (Tagged p) C(start a)
            -> RL (PatchInfoAnd p) C(a x)
            -> RL (PatchInfoAnd p) C(a c)
            -> ddd)
    -> ddd
with_partial_intersection (PatchSet ps1 NilRL) s j = j NilRL ps1 (newset2RL s)
with_partial_intersection s (PatchSet ps2 NilRL) j =
    j NilRL (newset2RL s) ps2
with_partial_intersection bbb (PatchSet a (Tagged ta _ _ :<: _)) j
    | Just (PatchSet b t) <- simpleTag (info ta) bbb = j t b (unsafeCoerceP a)
with_partial_intersection aaa (PatchSet b (Tagged tb _ pb :<: tbs)) j
    = case hopefullyM tb of
        Just _ -> with_partial_intersection aaa (PatchSet (b+<+tb:<:pb) tbs) j
        Nothing -> case splitOnTag (info tb) aaa of
                   PatchSet NilRL com :> us -> j com us (unsafeCoerceP b)
                   _ -> impossible

getPatchesBeyondTag :: RepoPatch p => PatchInfo -> PatchSet p C(start x) -> FlippedSeal (RL (PatchInfoAnd p)) C(x)
getPatchesBeyondTag t (PatchSet ps (Tagged hp _ _ :<:_)) | info hp == t = flipSeal ps
getPatchesBeyondTag t patchset@(PatchSet (hp:<:ps) ts) =
    if info hp == t
    then if getTagsRight patchset == [info hp]
         then flipSeal $ NilRL -- special case to avoid looking at redundant patches
         else case splitOnTag t patchset of _ :> e -> flipSeal e
    else case getPatchesBeyondTag t (PatchSet ps ts) of
         FlippedSeal xxs -> FlippedSeal (hp :<: xxs)
getPatchesBeyondTag t (PatchSet NilRL NilRL) = bug $ "tag\n" ++
                                                renderString (humanFriendly t) ++
                                                "\nis not in the patchset in getPatchesBeyondTag."
getPatchesBeyondTag t0 (PatchSet NilRL (Tagged t _ ps :<: ts)) =
                              getPatchesBeyondTag t0 (PatchSet (t:<:ps) ts)

splitOnTag :: RepoPatch p => PatchInfo -> PatchSet p C(start x) -> (PatchSet p :> RL (PatchInfoAnd p)) C(start x)
splitOnTag t (PatchSet ps (Tagged hp x ps2 :<: ts))
    | info hp == t = (PatchSet NilRL (Tagged hp x ps2 :<: ts)) :> ps
splitOnTag t patchset@(PatchSet (hp:<:ps) ts)
    | info hp == t = if getTagsRight patchset == [info hp]
                     then PatchSet NilRL (Tagged hp Nothing ps :<: ts) :> NilRL
                     else case partitionRL ((`notElem` (t:ds)) . info) (hp:<:ps) of
                          (x:<:a) :> b ->
                              if getTagsRight (PatchSet (x:<:a) ts) == [t]
                              then PatchSet NilRL (Tagged x Nothing a :<: ts) :> b
                              else case splitOnTag t $ eatOne $ PatchSet (x:<:a) ts of
                                   xx :> yy -> xx :> (b +<+ yy)
                          _ -> impossible
    where ds = getdeps (hopefully hp)
          eatOne :: PatchSet p C(start x) -> PatchSet p C(start x)
          eatOne (PatchSet ps1 (Tagged x _ ps2 :<: ts')) = PatchSet (ps1+<+x:<:ps2) ts'
          eatOne _ = bug "a stubborn case in splitOnTag (theoretically possible)"
splitOnTag t (PatchSet (p:<:ps) ts) = case splitOnTag t (PatchSet ps ts) of
                                        ns :> x -> ns :> (p:<:x)
splitOnTag t0 (PatchSet NilRL (Tagged t _ ps :<: ts)) = splitOnTag t0 (PatchSet (t:<:ps) ts)
splitOnTag t0 (PatchSet NilRL NilRL) = bug $ "tag\n" ++
                                                renderString (humanFriendly t0) ++
                                                "\nis not in the patchset in splitOnTag."

-- | @getPatchesInTag t ps@ returns a 'SealedPatchSet' of all
-- patches in @ps@ which are contained in @t@.
getPatchesInTag :: RepoPatch p => PatchInfo -> PatchSet p C(start x) -> SealedPatchSet p C(start)
getPatchesInTag t ns = case splitOnTag t ns of
                            ns' :> _ -> seal ns'

getTagsRight :: PatchSet p C(start x) -> [PatchInfo]
getTagsRight (PatchSet ps NilRL) = getTagsR (mapRL infoAndDeps ps)
getTagsRight (PatchSet ps (Tagged t _ _ :<: _)) = getTagsR (mapRL infoAndDeps (ps+<+t:<:NilRL))

getTagsR :: [(PatchInfo, Maybe [PatchInfo])] -> [PatchInfo]
getTagsR [] = []
getTagsR ((i0,Nothing):pps0) = i0 : getTagsR pps0
getTagsR ((i0,Just ds0):pps0) = i0 : getTagsR (drop_tags_r ds0 pps0)
    where
    drop_tags_r :: [PatchInfo]
                -> [(PatchInfo, Maybe [PatchInfo])] -> [(PatchInfo, Maybe [PatchInfo])]
    drop_tags_r [] pps = pps
    drop_tags_r _  []  = []
    drop_tags_r ds (hp:pps)
        | fst hp `elem` ds = case snd hp of
                             Just ds' -> drop_tags_r (ds'++delete (fst hp) ds) pps
                             Nothing -> drop_tags_r (delete (fst hp) ds) pps
        | otherwise = hp : drop_tags_r ds pps

infoAndDeps :: PatchInfoAnd p C(x y) -> (PatchInfo, Maybe [PatchInfo])
infoAndDeps p
        | isTag (info p) = (info p, getdeps `fmap` hopefullyM p)
        | otherwise = (info p, Nothing)

deepOptimizePatchset :: PatchSet p C(start x) -> PatchSet p C(start x)
deepOptimizePatchset ns = optimizePatchset (PatchSet (newset2RL ns) NilRL)

optimizePatchset :: PatchSet p C(start x) -> PatchSet p C(start x)
optimizePatchset (PatchSet NilRL ts) = PatchSet NilRL ts
optimizePatchset (PatchSet (p:<:ps) ts)
    | isTag (info p) && getTagsRight (PatchSet (p:<:ps) ts) == [info p]
      = case optimizePatchset (PatchSet ps ts) of
        PatchSet ps' ts' -> PatchSet NilRL (Tagged p Nothing ps' :<: ts')
    | otherwise = case optimizePatchset (PatchSet ps ts) of
                  PatchSet ps' ts' -> PatchSet (p:<:ps') ts'

slightlyOptimizePatchset :: PatchSet p C(start x) -> PatchSet p C(start x)
slightlyOptimizePatchset (PatchSet ps0 ts0) = sops $ PatchSet (progressRL "Optimizing inventory" ps0) ts0
    where sops :: PatchSet p C(start y) -> PatchSet p C(start y)
          sops (PatchSet NilRL ts) = PatchSet NilRL ts
          sops (PatchSet (hp:<:ps) ts)
              | isTag (info hp) = if getTagsRight (PatchSet (hp:<:ps) ts) == [info hp]
                                   then PatchSet NilRL (Tagged hp Nothing ps :<: ts)
                                   else case sops $ PatchSet (progressRL "Optimizing inventory" ps) ts of
                                        PatchSet ps' ts' -> PatchSet (hp:<:ps') ts'
              | otherwise = case sops $ PatchSet ps ts of
                            PatchSet ps' ts' -> PatchSet (hp:<:ps') ts'

commuteToEnd :: forall p C(start x y). RepoPatch p => RL (PatchInfoAnd p) C(x y)
               -> PatchSet p C(start y) -> (PatchSet p :> RL (PatchInfoAnd p)) C(start x)
commuteToEnd NilRL (PatchSet ps ts) = PatchSet NilRL ts :> ps
commuteToEnd (p:<:ps) (PatchSet xs ts)
    | info p `elem` mapRL info xs = case fastRemoveRL p xs of
                                    Just xs' -> commuteToEnd ps (PatchSet xs' ts)
                                    Nothing -> impossible -- "Nothing is impossible"
commuteToEnd ps (PatchSet xs (Tagged t _ ys :<: ts)) =
    commuteToEnd ps (PatchSet (xs+<+t:<:ys) ts)
commuteToEnd _ _ = impossible

removeFromPatchSet :: RepoPatch p => FL (PatchInfoAnd p) C(x y)
                 -> PatchSet p C(start y) -> Maybe (PatchSet p C(start x))
removeFromPatchSet bad0 = rfns (reverseFL bad0)
    where rfns :: RepoPatch p => RL (PatchInfoAnd p) C(x y)
               -> PatchSet p C(start y) -> Maybe (PatchSet p C(start x))
          rfns bad (PatchSet ps ts)
              | all (`elem` (mapRL info ps)) (mapRL info bad) =
                  do ps' <- removeSubsequenceRL bad ps
                     Just $ PatchSet ps' ts
          rfns _ (PatchSet _ NilRL) = Nothing
          rfns bad (PatchSet ps (Tagged t _ tps :<: ts)) =
                        rfns bad (PatchSet (ps+<+t:<:tps) ts)

findCommonWithThem :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y)
                   -> (PatchSet p :> FL (PatchInfoAnd p)) C(start x)
findCommonWithThem us them =
    with_partial_intersection us them $
    \common us' them' ->
        case partitionFL ((`elem` mapRL info them') . info) $ reverseRL us' of
          _ :> bad@(_:>:_) :> _ -> bug $ "Failed to commute common patches:\n" ++
                                   (renderString $ vcat $ mapRL (humanFriendly . info) $ reverseFL bad)
          common2 :> _nilfl :> only_ours -> PatchSet (reverseFL common2) common :> unsafeCoerceP only_ours

findUncommon :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y)
                         -> (FL (PatchInfoAnd p) :\/: FL (PatchInfoAnd p)) C(x y)
findUncommon us them =
  case findCommonWithThem us them of
    _common :> us' -> case findCommonWithThem them us of
      _ :> them' -> unsafeCoercePStart us' :\/: them'

countUsThem :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y) -> (Int, Int)
countUsThem us them =
    with_partial_intersection us them $
    \_ us' them' -> let uu = mapRL info us'
                        tt = mapRL info them'
                    in (length $ uu \\ tt, length $ tt \\ uu)

mergeThem :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y)
           -> Sealed (FL (PatchInfoAnd p) C(x))
mergeThem us them =
   with_partial_intersection us them $
    \_ us' them' -> merge2FL (reverseRL us') (reverseRL them')

newsetIntersection :: RepoPatch p => [SealedPatchSet p C(start)] -> SealedPatchSet p C(start)
newsetIntersection [] = seal $ PatchSet NilRL NilRL
newsetIntersection [x] = x
newsetIntersection (Sealed y:ys) =
    case newsetIntersection ys of
    Sealed z -> with_partial_intersection y z $
                \common a b ->
                    case mapRL info a `intersect` mapRL info b of
                    morecommon ->
                        case partitionRL (\e -> info e `notElem` morecommon) a of
                        commonps :> _ -> seal $ PatchSet commonps common

newsetUnion :: RepoPatch p => [SealedPatchSet p C(start)] -> SealedPatchSet p C(start)
newsetUnion [] = seal $ PatchSet NilRL NilRL
newsetUnion [x] = x
newsetUnion (Sealed y@(PatchSet psy tsy):Sealed y2:ys) =
    case mergeThem y y2 of
    Sealed p2 -> newsetUnion $ seal (PatchSet (reverseFL p2+<+psy) tsy) : ys

-- | Merge two FLs (say L and R), starting in a common context. The result is a
-- FL starting in the original end context of L, going to a new context that is
-- the result of applying all patches from R on top of patches from L.
--
-- While this function is similar to 'mergeFL', there are three important
-- differences to keep in mind:
--
-- * 'mergeFL' does not correctly deal with duplicate patches whereas this one
--   does
--   (Question from Eric Kow: in what sense? Why not fix the mergeFL instance?)
--
-- * 'mergeFL' returns both paths of the merge diamond, but this version only
--   returns one, so you'd better choose the order carefully, eg.
--   (@merge2FL l r@)
--
-- * The conventional order we use in this function is reversed from
--   'mergeFL' (so @mergeFL r l@ vs. @merge2FL l r@. This does not
--   matter so much for the former since you get both paths.
--   (Question from Eric Kow: should we flip merge2FL for more uniformity in
--    the code?)
merge2FL :: RepoPatch p => FL (PatchInfoAnd p) C(x y)
         -> FL (PatchInfoAnd p) C(x z)
         -> Sealed (FL (PatchInfoAnd p) C(y))
merge2FL _ NilFL = seal NilFL
merge2FL NilFL ys = seal ys
merge2FL xs (y:>:ys) | Just xs' <- fastRemoveFL y xs = merge2FL xs' ys
merge2FL (x:>:xs) ys | Just ys' <- fastRemoveFL x ys = merge2FL xs ys'
                     | otherwise = case mergeFL (x :\/: ys) of
                                     ys' :/\: _ -> merge2FL xs ys'

simpleTag :: PatchInfo -> PatchSet p C(start x) -> Maybe (PatchSet p C(start x))
simpleTag t0 (PatchSet ps (Tagged t h pst :<: ts))
    | t0 == info t = Just $ PatchSet ps (Tagged t h pst :<: ts)
    | otherwise = do PatchSet ps' ts' <- simpleTag t0 (PatchSet (t:<:pst) ts)
                     Just $ PatchSet (ps +<+ ps') ts'
simpleTag _ _ = Nothing

areUnrelatedRepos :: RepoPatch p => PatchSet p C(start x) -> PatchSet p C(start y) -> Bool
areUnrelatedRepos us them =
    with_partial_intersection us them checkit
    where checkit (Tagged _ _ _ :<: _) _ _ = False
          checkit _ u t | t `isShorterThanRL` 5 = False
                        | u `isShorterThanRL` 5 = False
                        | otherwise = null $ intersect (mapRL info u) (mapRL info t)

-- | Remove a patch from FL, using PatchInfo equality. The result is Just
-- whenever the patch has been found and removed. If the patch is not present
-- in the sequence at all or any commutation fails, we get Nothing. First two
-- cases are optimisations for the common cases where the head of the list is
-- the patch to remove, or the patch is not there at all.
fastRemoveFL :: RepoPatch p => PatchInfoAnd p C(x y) -> FL (PatchInfoAnd p) C(x z)
             -> Maybe (FL (PatchInfoAnd p) C(y z))
fastRemoveFL _ NilFL = Nothing
fastRemoveFL a (b:>:bs) | IsEq <- a =\/= b = Just bs
                        | info a `notElem` mapFL info bs = Nothing
fastRemoveFL a (b:>:bs) = do a' :> bs' <- pullout NilRL bs
                             a'' :> b' <- commute (b :> a')
                             IsEq <- return (a'' =\/= a)
                             Just (b':>:bs')
    where i = info a
          pullout :: RepoPatch p => RL (PatchInfoAnd p) C(a0 a)
                  -> FL (PatchInfoAnd p) C(a b)
                  -> Maybe ((PatchInfoAnd p :> FL (PatchInfoAnd p)) C(a0 b))
          pullout _ NilFL = Nothing
          pullout acc (x:>:xs) | info x == i = do x' :> acc' <- commuteRL (acc :> x)
                                                  Just (x' :> reverseRL acc' +>+ xs)
                               | otherwise = pullout (x:<:acc) xs

fastRemoveRL :: RepoPatch p => PatchInfoAnd p C(y z) -> RL (PatchInfoAnd p) C(x z)
             -> Maybe (RL (PatchInfoAnd p) C(x y))
fastRemoveRL _ NilRL = Nothing
fastRemoveRL a (b:<:bs) | IsEq <- a =/\= b = Just bs
                        | info a `notElem` mapRL info bs = Nothing
fastRemoveRL a (b:<:bs) = do bs' :> a' <- pullout NilFL bs
                             b' :> a'' <- commute (a' :> b)
                             IsEq <- return (a'' =/\= a)
                             Just (b':<:bs')
    where i = info a
          pullout :: RepoPatch p => FL (PatchInfoAnd p) C(b c)
                  -> RL (PatchInfoAnd p) C(a b)
                  -> Maybe ((RL (PatchInfoAnd p) :> PatchInfoAnd p) C(a c))
          pullout _ NilRL = Nothing
          pullout acc (x:<:xs) | info x == i = do acc' :> x' <-
                                                      either (const Nothing)
                                                      Just (commuteFLorComplain (x :> acc))
                                                  Just (reverseFL acc' +<+ xs :> x')
                               | otherwise = pullout (x:>:acc) xs
