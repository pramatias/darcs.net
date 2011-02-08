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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

-- | Conflictor patches
module Darcs.Patch.V2.Real
       ( RealPatch(..), prim2real, isConsistent, isForward, isDuplicate,
         mergeUnravelled ) where

import Control.Monad ( mplus, liftM )
import Data.List ( partition, nub )
import Darcs.Patch.Commute ( commuteFLorComplain, commuteRL, commuteRLFL )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts(..), IsConflictedPrim(..), ConflictState(..) )
import Darcs.Patch.ConflictMarking ( mangleUnravelled )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(..), FileNameFormat(NewFormat) )
import Darcs.Patch.Invert ( invertFL, invertRL )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Prim ( FromPrim(..), ToFromPrim(..),
                          showPrim, showPrimFL,
                          readPrim,
                          PrimOf, PrimPatchBase, PrimPatch
                        )
import Darcs.Patch.Read ( bracketedFL )
import Darcs.Patch.Repair ( mapMaybeSnd, RepairToFL(..), Check(..) )
import Darcs.Patch.Patchy ( Patchy, Apply(..), Commute(..)
                          , PatchInspect(..)
                          , ReadPatch(..), ShowPatch(..)
                          , Invert(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..) )
import Darcs.Patch.Summary ( plainSummary )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (+>+)
                               , (+<+), mapFL_FL, reverseFL, (:\/:)(..)
                               , (:/\:)(..), reverseRL, lengthFL
                               , lengthRL )
import Darcs.Patch.V2.Non ( Non(..), Nonable(..), unNon,
                            showNons, showNon, readNons, readNon,
                            add, addP, addPs, remP, remPs, remNons,
                            (*>), (>*), (*>>), (>>*) )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL, commuteWhatWeCanRL,
                                  genCommuteWhatWeCanRL,
                                  removeRL, removeFL, removeSubsequenceFL )
import Darcs.Patch.RepoPatch ()
import qualified Data.ByteString.Char8 as BC ( ByteString, pack )
import Darcs.Patch.ReadMonads ( skipSpace, string, choice )
import Darcs.Utils ( nubsort )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Sealed ( FlippedSeal(..), Sealed(Sealed), mapSeal )
import Darcs.Witnesses.Show ( Show2(..), ShowDict(..) )
import Printer ( Doc, renderString, blueText, redText, (<+>), ($$) )
import Darcs.ColorPrinter ( errorDoc, assertDoc )
--import Printer ( greenText )
--import Darcs.ColorPrinter ( traceDoc )
#include "impossible.h"

-- |
-- @Duplicate x@: This patch has no effect since @x@ is already present in the repository
--
-- @Etacilpud x: invert (Duplicate x)@
--
-- @Normal prim@: A primitive patch
--
-- @Conflictor ix xx x@:
-- @ix@ is the set of patches:
--
--   * that conflict with @x@ and also conflict with another patch in the repository
--
--   * that conflict with a patch that conflict with @x@
--
-- @xx@ is the sequence of patches that conflict *only* with @x@
--
-- @x@ is the current patch
--
-- @ix@ and @x@ are stored as @Non@ objects, which include any necessary
--  context to uniquely define the patch that is referred to.
--
-- @InvConflictor ix xx x@: like @invert (Conflictor ix xx x)@
data RealPatch prim C(x y) where
    Duplicate :: Non (RealPatch prim) C(x) -> RealPatch prim C(x x)
    Etacilpud :: Non (RealPatch prim) C(x) -> RealPatch prim C(x x)
    Normal :: prim C(x y) -> RealPatch prim C(x y)
    Conflictor :: [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> Non (RealPatch prim) C(x) -> RealPatch prim C(y x)
    InvConflictor :: [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> Non (RealPatch prim) C(x) -> RealPatch prim C(x y)

instance PrimPatch prim => PrimPatchBase (RealPatch prim) where
   type PrimOf (RealPatch prim) = prim

-- | 'isDuplicate' @p@ is ' @True@ if @p@ is either a  'Duplicate' or 'Etacilpud' patch
isDuplicate :: RealPatch prim C(s y) -> Bool
isDuplicate (Duplicate _) = True
isDuplicate (Etacilpud _) = True
isDuplicate _ = False

-- | This is only used for unit testing
isForward :: PrimPatch prim => RealPatch prim C(s y) -> Maybe Doc
isForward p@(InvConflictor _ _ _) =
    Just $ redText "An inverse conflictor" $$ showPatch p
isForward p@(Etacilpud _) =
    Just $ redText "An inverse duplicate" $$ showPatch p
isForward _ = Nothing

mergeUnravelled :: PrimPatch prim => [Sealed ((FL prim) C(x))] -> Maybe (FlippedSeal (RealPatch prim) C(x))
mergeUnravelled [] = Nothing
mergeUnravelled [_] = Nothing
mergeUnravelled ws = case mergeUnravelled_private ws of
                     Nothing -> Nothing
                     Just NilRL -> bug "found no patches in mergeUnravelled"
                     Just (z:<:_) -> Just $ FlippedSeal z
    where notNullS :: PrimPatch prim => Sealed ((FL prim) C(x)) -> Bool
          notNullS (Sealed NilFL) = False
          notNullS _ = True
          mergeUnravelled_private :: PrimPatch prim => [Sealed (FL prim C(x))] -> Maybe (RL (RealPatch prim) C(x x))
          mergeUnravelled_private xs = reverseFL `fmap` mergeConflictingNons
                                                        (map sealed2non $ filter notNullS xs)

-- | 'sealed2non' @(Sealed xs)@ converts @xs@ to a 'Non'.
--   @xs@ must be non-empty since we split this list at the last patch
sealed2non :: Sealed ((FL prim) C(x)) -> Non (RealPatch prim) C(x)
sealed2non (Sealed xs) = case reverseFL xs of
                         y:<:ys -> Non (mapFL_FL fromPrim $ reverseRL ys) y
                         NilRL -> bug "NilFL encountered in sealed2non"

mergeConflictingNons :: PrimPatch prim => [Non (RealPatch prim) C(x)] -> Maybe (FL (RealPatch prim) C(x x))
mergeConflictingNons ns = mcn $ map unNon ns
    where mcn :: PrimPatch prim => [Sealed (FL (RealPatch prim) C(x))] -> Maybe (FL (RealPatch prim) C(x x))
          mcn [] = Just NilFL
          mcn [Sealed p] = case joinEffects p of -- this is just a safety check, and could
                           NilFL -> Just p                 -- be removed when we're sure of the code.
                           _ -> Nothing
          mcn (Sealed p1:Sealed p2:zs) = case pullCommon p1 p2 of
                                         Common c ps qs ->
                                             case merge (ps :\/: qs) of
                                             qs' :/\: _ -> mcn (Sealed (c +>+ ps +>+ qs'):zs)

joinEffects :: forall p C(x y)
             . (Effect p, Invert (PrimOf p), Commute (PrimOf p), MyEq (PrimOf p))
            => p C(x y) -> FL (PrimOf p) C(x y)
joinEffects = joinInverses . effect
    where joinInverses :: FL (PrimOf p) C(a b) -> FL (PrimOf p) C(a b)
          joinInverses NilFL = NilFL
          joinInverses (p:>:ps) = case removeFL (invert p) ps' of
                                   Just ps'' -> ps''
                                   Nothing -> p :>: ps'
              where ps' = joinInverses ps

assertConsistent :: PrimPatch prim => RealPatch prim C(x y) -> RealPatch prim C(x y)
assertConsistent x = assertDoc (do e <- isConsistent x
                                   Just (redText "Inconsistent patch:" $$ showPatch x $$ e)) x

-- | @mergeAfterConflicting@ takes as input a sequence of conflicting
-- patches @xxx@ (which therefore have no effect) and a sequence of
-- primitive patches @yyy@ that follow said sequence of conflicting
-- patches, and may depend upon some of the conflicting patches (as a
-- resolution).

-- The output is two sequences of patches the first consisting of a
-- set of mutually-conflicting patches, and the second having the same
-- effect as the original primitive patch sequence in the input.

-- So far as I can tell, the second output is always identical to
-- @mapFL Normal yyy@

-- The first output is the set of patches from @xxx@ that are depended
-- upon by @yyy@.

mergeAfterConflicting :: PrimPatch prim
                      => FL (RealPatch prim) C(x x) -> FL prim C(x y)
                      -> Maybe (FL (RealPatch prim) C(x x), FL (RealPatch prim) C(x y))
mergeAfterConflicting xxx yyy = --traceDoc (greenText "mergeAfterConflicting xxx" $$ showPatch xxx $$
                                --          greenText "and yyy" $$ showPatch yyy) $
                                mac (reverseFL xxx) yyy NilFL
    where mac :: PrimPatch prim
              => RL (RealPatch prim) C(x y) -> FL prim C(y z) -> FL (RealPatch prim) C(z a)
              -> Maybe (FL (RealPatch prim) C(x x), FL (RealPatch prim) C(x a))
          mac NilRL xs goneby = case joinEffects goneby of
                                NilFL -> Just (NilFL, mapFL_FL Normal xs)
                                _z -> --traceDoc (greenText "mac1 z" $$ showPatch _z) $
                                      Nothing
          mac (p:<:ps) xs goneby = --traceDoc (greenText "mac ps" $$ showPatch ps $$
                                   --          greenText "p" $$ showPatch p $$
                                   --          greenText "xs" $$ showPatch xs $$
                                   --          greenText "goneby" $$ showPatch goneby) $
                                   case commuteFLorComplain (p :> mapFL_FL Normal xs) of
                                   Left _  -> case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                                              a:>p':>b ->
                                                  do (b',xs') <- mac b xs goneby
                                                     let pa = joinEffects $ p':<:a
                                                     --traceDoc (greenText "foo1" $$
                                                     --          showPatch pa) $ Just ()
                                                     NilFL <- return pa
                                                     return (reverseRL (p':<:a)+>+b', xs')
                                                   `mplus` do NilFL <- return goneby
                                                              NilFL <- return $ joinEffects (p:<:ps)
                                                              return (reverseRL (p:<:ps),
                                                                      mapFL_FL Normal xs)
                                   Right (l:>p'') ->
                                       case allNormal l of
                                       Just xs'' -> mac ps xs'' (p'':>:goneby)
                                       Nothing ->
                                              case genCommuteWhatWeCanRL commuteNoConflicts (ps :> p) of
                                              a:>p':>b ->
                                                  do (b',xs') <- mac b xs goneby
                                                     let pa = joinEffects $ p':<:a
                                                     --traceDoc (greenText "foo2" $$
                                                     --          showPatch pa) $ Just ()
                                                     NilFL <- return pa
                                                     return $ (reverseRL (p':<:a)+>+b', xs')

geteff :: PrimPatch prim => [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> ([Non (RealPatch prim) C(x)], FL (RealPatch prim) C(x y))
geteff _ NilFL = ([],NilFL)
geteff ix (x:>:xs) | Just ix' <- mapM (remP (Normal x)) ix
                               = --traceDoc (greenText "I got rid of x" $$ showPatch x) $
                                 case geteff ix' xs of
                                 (ns,xs') -> (non (Normal x) : map (addP (Normal x)) ns,
                                              Normal x :>: xs')
geteff ix xx = case mergeConflictingNons ix of
               Nothing -> errorDoc $ redText "mergeConflictingNons failed in geteff with ix" $$
                          showNons ix $$ redText "xx" $$ showPatch xx
               Just rix -> case mergeAfterConflicting rix xx of
                           Just (a,x) -> (map (addPs (reverseFL a)) $ toNons x,
                                          a +>+ x)
                           Nothing -> errorDoc $ redText "mergeAfterConflicting failed in geteff"$$
                                      redText "where ix" $$ showNons ix $$
                                      redText "and xx" $$ showPatch xx $$
                                      redText "and rix" $$ showPatch rix

xx2nons :: PrimPatch prim => [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> [Non (RealPatch prim) C(x)]
xx2nons ix xx = fst $ geteff ix xx

xx2patches :: PrimPatch prim => [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> FL (RealPatch prim) C(x y)
xx2patches ix xx = snd $ geteff ix xx

-- | If @xs@ consists only of 'Normal' patches, 'allNormal' @xs@ returns
--   @Just pxs@ those patches (so @lengthFL pxs == lengthFL xs@).
--   Otherwise, it returns 'Nothing'.
allNormal :: FL (RealPatch prim) C(x y) -> Maybe (FL prim C(x y))
allNormal (Normal x:>:xs) = (x :>:) `fmap` allNormal xs
allNormal NilFL = Just NilFL
allNormal _ = Nothing

-- | This is used for unit-testing and for internal sanity checks
isConsistent :: PrimPatch prim => RealPatch prim C(x y) -> Maybe Doc
isConsistent (Normal _) = Nothing
isConsistent (Duplicate _) = Nothing
isConsistent (Etacilpud _) = Nothing
isConsistent (Conflictor im mm m@(Non deps _))
    | not $ everyoneConflicts im = Just $ redText "Someone doesn't conflict in im in isConsistent"
    | Just _ <- remPs rmm m, _:>:_ <- mm = Just $ redText "m doesn't conflict with mm in isConsistent"
    | any (\x -> any (x `conflictsWith`) nmm) im
        = Just $ redText "mm conflicts with im in isConsistent where nmm is" $$
                 showNons nmm
    | Nothing <- (nmm ++ im) `minus` toNons deps = Just $ redText "dependencies not in conflict:" $$
                                                   showNons (toNons deps) $$
                                                   redText "compared with deps itself:" $$
                                                   showPatch deps
    | otherwise = case allConflictsWith m im of
                  (im1,[]) | im1 `eqSet` im -> Nothing
                  (_,imnc) -> Just $ redText "m doesn't conflict with im in isConsistent.  unconflicting:"
                              $$ showNons imnc
    where (nmm, rmm) = geteff im mm
isConsistent c@(InvConflictor _ _ _) = isConsistent (invert c)

everyoneConflicts :: PrimPatch prim => [Non (RealPatch prim) C(x)] -> Bool
everyoneConflicts [] = True
everyoneConflicts (x:xs) = case allConflictsWith x xs of
                            ([],_) -> False
                            (_,xs') -> everyoneConflicts xs'

prim2real :: prim C(x y) -> RealPatch prim C(x y)
prim2real = Normal

instance PrimPatch prim => Patchy (RealPatch prim)

mergeWith :: PrimPatch prim => Non (RealPatch prim) C(x) -> [Non (RealPatch prim) C(x)] -> Sealed (FL prim C(x))
mergeWith p [] = effect `mapSeal` unNon p
mergeWith p xs = mergeall $ map unNon $ (p:) $ unconflicting_of $
                  filter (\x -> not (p `dependsUpon` x) && not (p `conflictsWith` x)) xs
    where mergeall :: PrimPatch prim => [Sealed (FL (RealPatch prim) C(x))] -> Sealed (FL prim C(x))
          mergeall [Sealed x] = Sealed $ effect x
          mergeall [] = Sealed NilFL
          mergeall (Sealed x:Sealed y:rest) = case merge (x :\/: y) of
                                              y' :/\: _ -> mergeall (Sealed (x+>+y'):rest)
          unconflicting_of [] = []
          unconflicting_of (q:qs) = case allConflictsWith q qs of
                                    ([],_) -> q:qs
                                    (_,nc) -> unconflicting_of nc

instance PrimPatch prim => Conflict (RealPatch prim) where
    conflictedEffect (Duplicate (Non _ x)) = [IsC Duplicated x]
    conflictedEffect (Etacilpud _) = impossible
    conflictedEffect (Conflictor _ _ (Non _ x)) = [IsC Conflicted x]
    conflictedEffect (InvConflictor _ _ _) = impossible
    conflictedEffect (Normal x) = [IsC Okay x]
    resolveConflicts (Conflictor ix xx x) = [mangleUnravelled unravelled : unravelled]
            where unravelled = nub $ filter isn $ map (`mergeWith` (x:ix++nonxx)) (x:ix++nonxx)
                  nonxx = nonxx_ (nonxx_aux ix xx)
                  nonxx_aux :: [Non (RealPatch prim) C(x)] -> FL prim C(x y) -> RL (RealPatch prim) C(x y)
                  nonxx_aux a b = reverseFL $ xx2patches a b
                  nonxx_ :: RL (RealPatch prim) C(x y) -> [Non (RealPatch prim) C(x)]
                  nonxx_ NilRL = []
                  nonxx_ ((Normal q) :<: qs) = [Non (reverseRL qs) q]
                  nonxx_ _ = []
                  isn :: Sealed (FL p C(x)) -> Bool
                  isn (Sealed NilFL) = False
                  isn _ = True
    resolveConflicts _ = []

instance PrimPatch prim => CommuteNoConflicts (RealPatch prim) where
    -- cA
    commuteNoConflicts (Duplicate x :> Duplicate y) = Just (Duplicate y :> Duplicate x)
    commuteNoConflicts (Etacilpud x :> Duplicate y) = Just (Duplicate y :> Etacilpud x)
    commuteNoConflicts (Duplicate x :> Etacilpud y) = Just (Etacilpud y :> Duplicate x)
    commuteNoConflicts (Etacilpud x :> Etacilpud y) = Just (Etacilpud y :> Etacilpud x)
    -- cB
    commuteNoConflicts (x :> Duplicate d) = if d == addP (invert x) (non x)
                                              then Just (x :> Duplicate d)
                                              else do d' <- remP (invert x) d
                                                      return (Duplicate d' :> x)
    commuteNoConflicts (Duplicate d' :> x) = Just (x :> Duplicate (addP (invert x) d'))
    commuteNoConflicts c@(Etacilpud _ :> _) = invertCommuteNC c
    commuteNoConflicts c@(_ :> Etacilpud _) = invertCommuteNC c
    -- cE
    commuteNoConflicts (Normal x :> Normal y) =   do y' :> x' <- commute (x :> y)
                                                     return (Normal y' :> Normal x')
    -- cF -- involves a conflict
    -- cG
    commuteNoConflicts (Normal x :> Conflictor iy yy y) =
        case commuteFLorComplain (x :> invert yy) of
        Right (iyy' :> x') -> do
           y':iy' <- mapM (Normal x' >*) (y:iy)
           return (Conflictor iy' (invert iyy') y' :> Normal x')
        _ -> Nothing
    -- cFi+cGi  -- handle with previous two pattern matches
    commuteNoConflicts c@(InvConflictor _ _ _ :> Normal _) = invertCommuteNC c
    -- icG FIXME: where is icF?
    commuteNoConflicts (Conflictor iy' yy' y' :> Normal x') =
        do x :> iyy <- commuteRL (invertFL yy' :> x')
           y:iy <- mapM (*> Normal x') (y':iy')
           return (Normal x :> Conflictor iy (invertRL iyy) y)
    -- icGi      -- handle with previous pattern match
    commuteNoConflicts c@(Normal _ :> InvConflictor _ _ _) = invertCommuteNC c
    -- cH -- this involves a conflict commute
    -- cI
    commuteNoConflicts (Conflictor ix xx x :> Conflictor iy yy y) =
        do xx' :> yy' <- commute (yy :> xx)
           x':ix' <- mapM (yy >>*) (x:ix)
           y':iy' <- mapM (*>> xx') (y:iy)
           False <- return $ any (conflictsWith y) (x':ix')
           False <- return $ any (conflictsWith x') iy
           return (Conflictor iy' yy' y' :> Conflictor ix' xx' x')
    -- cHi+cIi            uses previous two matches
    commuteNoConflicts c@(InvConflictor _ _ _ :> InvConflictor _ _ _) = invertCommuteNC c
    -- cJ
    commuteNoConflicts (InvConflictor ix xx x :> Conflictor iy yy y) =
        do iyy' :> xx' <- commute (xx :> invert yy)
           y':iy' <- mapM (xx' >>*) (y:iy)
           x':ix' <- mapM (invertFL iyy' >>*) (x:ix)
           False <- return $ any (conflictsWith y') (x':ix')
           False <- return $ any (conflictsWith x') iy'
           return (Conflictor iy' (invert iyy') y' :> InvConflictor ix' xx' x')
    -- icJ
    commuteNoConflicts (Conflictor iy' yy' y' :> InvConflictor ix' xx' x') =
        do xx :> iyy <- commute (invert yy' :> xx')
           y:iy <- mapM (*>> xx') (y':iy')
           x:ix <- mapM (*>> yy') (x':ix')
           False <- return $ any (conflictsWith y') (x':ix')
           False <- return $ any (conflictsWith x') iy'
           return (InvConflictor ix xx x :> Conflictor iy (invert iyy) y)

instance PrimPatch prim => Check (RealPatch prim) where
    isInconsistent = isConsistent

instance FromPrim (RealPatch prim) where
    fromPrim = prim2real
instance ToFromPrim (RealPatch prim) where
    toPrim (Normal p) = Just p
    toPrim _ = Nothing

instance PrimPatch prim => MyEq (RealPatch prim) where
    (Duplicate x) =\/= (Duplicate y) | x == y = IsEq
    (Etacilpud x) =\/= (Etacilpud y) | x == y = IsEq
    (Normal x) =\/= (Normal y) = x =\/= y
    (Conflictor cx xx x) =\/= (Conflictor cy yy y)
        | map (add $ invertFL xx) cx `eqSet`
          map (add $ invertFL yy) cy &&
          add (invert xx) x == add (invert yy) y = xx =/\= yy
    (InvConflictor cx xx x) =\/= (InvConflictor cy yy y)
        | cx `eqSet` cy && x == y = xx =\/= yy
    _ =\/= _ = NotEq

eqSet :: Eq a => [a] -> [a] -> Bool
eqSet [] [] = True
eqSet (x:xs) xys | Just ys <- remove1 x xys = eqSet xs ys
eqSet _ _ = False

remove1 :: Eq a => a -> [a] -> Maybe [a]
remove1 x (y:ys) | x == y = Just ys
                 | otherwise = (y :) `fmap` remove1 x ys
remove1 _ [] = Nothing

minus :: Eq a => [a] -> [a] -> Maybe [a]
minus xs [] = Just xs
minus xs (y:ys) = do xs' <- remove1 y xs
                     xs' `minus` ys

invertNon :: PrimPatch prim => Non (RealPatch prim) C(x) -> Non (RealPatch prim) C(x)
invertNon (Non c x)
    | Just rc' <- removeRL nix (reverseFL c) = Non (reverseRL rc') (invert x)
    | otherwise = addPs (Normal x :<: reverseFL c) $ non nix
    where nix = Normal $ invert x

nonTouches :: PatchInspect prim => Non (RealPatch prim) C(x) -> [FilePath]
nonTouches (Non c x) = listTouchedFiles (c +>+ fromPrim x :>: NilFL)

nonHunkMatches :: PatchInspect prim => (BC.ByteString -> Bool) -> Non (RealPatch prim) C(x) -> Bool
nonHunkMatches f (Non c x) = hunkMatches f c || hunkMatches f x

toNons :: forall p C(x y)
        . (Conflict p, Patchy p, PatchListFormat p, ToFromPrim p,
           Nonable p, ShowPatchBasic (PrimOf p))
       => FL p C(x y) -> [Non p C(x)]
toNons xs = map lastNon $ initsFL xs
    where lastNon :: Sealed ((p :> FL p) C(x)) -> Non p C(x)
          lastNon (Sealed xxx) = case lastNon_aux xxx of
                                 deps :> p :> _ -> case non p of
                                                   Non NilFL pp -> Non (reverseRL deps) pp
                                                   Non ds pp -> errorDoc $ redText "Weird case in toNons" $$
                                                                redText "please report this bug!" $$
                                                                (case xxx of
                                                                 z:>zs -> showPatch (z:>:zs)) $$
                                                                redText "ds are" $$ showPatch ds $$
                                                                redText "pp is" $$ showPatch pp
          reverseFoo :: (p :> FL p) C(x z) -> (RL p :> p) C(x z)
          reverseFoo (p :> ps) = rf NilRL p ps
              where rf :: RL p C(a b) -> p C(b c) -> FL p C(c d) -> (RL p :> p) C(a d)
                    rf rs l NilFL = rs :> l
                    rf rs x (y:>:ys) = rf (x:<:rs) y ys
          lastNon_aux :: (p :> FL p) C(x z) -> (RL p :> p :> RL p) C(x z)
          lastNon_aux = commuteWhatWeCanRL . reverseFoo

initsFL :: Patchy p => FL p C(x y) -> [Sealed ((p :> FL p) C(x))]
initsFL NilFL = []
initsFL (x:>:xs) = Sealed (x:>NilFL) : map (\ (Sealed (y:>xs')) -> Sealed (x:>y:>:xs')) (initsFL xs)

filterConflictsFL :: PrimPatch prim => Non (RealPatch prim) C(x) -> FL prim C(x y) -> (FL prim :> FL prim) C(x y)
filterConflictsFL _ NilFL = NilFL :> NilFL
filterConflictsFL n (p:>:ps)
    | Just n' <- remP (fromPrim p) n = case filterConflictsFL n' ps of
                                       p1 :> p2 -> p:>:p1 :> p2
    | otherwise = case commuteWhatWeCanFL (p :> ps) of
                  p1 :> p' :> p2 -> case filterConflictsFL n p1 of
                                    p1a :> p1b -> p1a :> p1b +>+ p' :>: p2

instance Invert prim => Invert (RealPatch prim) where
    invert (Duplicate d) = Etacilpud d
    invert (Etacilpud d) = Duplicate d
    invert (Normal p) = Normal (invert p)
    invert (Conflictor x c p) = InvConflictor x c p
    invert (InvConflictor x c p) = Conflictor x c p

instance PrimPatch prim => Commute (RealPatch prim) where
--    commute (x :> y) | traceDoc (greenText "commuting x" $$ showPatch x $$
--                                 greenText "with y" $$ showPatch y) False = undefined
    commute (x :> y) | Just (y' :> x') <- commuteNoConflicts (assertConsistent x :> assertConsistent y) = Just (y' :> x')
    -- cF
    commute (Normal x :> Conflictor a1'nop2 n1'x p1') -- these patches conflicted
        | Just rn1' <- removeRL x (reverseFL n1'x) =
                      do let p2:n1nons = reverse $ xx2nons a1'nop2 $ reverseRL (x:<:rn1')
                             a2 = p1':a1'nop2++n1nons
                         case (a1'nop2, reverseRL rn1', p1') of
                           ([], NilFL, Non c y) | NilFL <- joinEffects c ->
                                    Just (Normal y :> Conflictor a1'nop2 (y:>:NilFL) p2)
                           (a1,n1,_) -> Just (Conflictor a1 n1 p1' :> Conflictor a2 NilFL p2)
    -- cFi  -- handle with previous pattern match
    commute c@(InvConflictor _ _ _ :> Normal _) = invertCommute c
    -- cH
    commute (Conflictor a1 n1 p1 :> Conflictor a2 n2 p2)
        | Just a2_minus_p1 <- remove1 p1' a2,
          not (p2 `dependsUpon` p1') =
              do let n1nons = map (add n2) $ xx2nons a1 n1
                     n2nons = xx2nons a2 n2
                     Just a2_minus_p1n1 = a2_minus_p1 `minus` n1nons
                     n2n1 = n2 +>+ n1
                     a1' = map (add n2) a1
                     p2ooo = remNons a1' p2
                 n1' :> n2' <- return $ filterConflictsFL p2ooo n2n1
                 let n1'n2'nons = xx2nons a2_minus_p1n1 (n1'+>+n2')
                     n1'nons = take (lengthFL n1') n1'n2'nons
                     n2'nons = drop (lengthFL n1') n1'n2'nons
                     Just a1'nop2 = (a2++n2nons) `minus` (p1':n1'nons)
                     Just a2'o = --traceDoc (greenText "\n\nConflictor a1 n1 p1 is" $$
                                 --          showPatch (assertConsistent $ Conflictor a1 n1 p1) $$
                                 --          greenText "and Conflictor a2 n2 p2 is" $$
                                 --          showPatch (assertConsistent $ Conflictor a2 n2 p2) $$
                                 --          greenText "where n2'nons is" $$ showNons n2'nons $$
                                 --          greenText "and others are" $$
                                 --          showNons (fst $ allConflictsWith p2 $ a2_minus_p1++n2nons) $$
                                 --          greenText "These came from" $$
                                 --          showNons (a2_minus_p1++n2nons) $$
                                 --          greenText "n1'n2'nons" $$ showNons n1'n2'nons $$
                                 --          greenText "from n1' :> n2'" $$
                                 --          showPatch n1' $$ greenText ":>" $$ showPatch n2' $$
                                 --          greenText "p2" $$ showNon p2 $$
                                 --          greenText "p2 fixed" $$ showNon p2ooo $$
                                 --          -- greenText "pren1" $$ showPatch pren1 $$
                                 --          greenText "n1'" $$ showPatch n1' $$
                                 --          greenText "p2" $$ showNon p2
                                 --         )
                                 (fst $ allConflictsWith p2 $ a2_minus_p1++n2nons) `minus` n2'nons
                     Just a2' = mapM (remPs (xx2patches a1'nop2 n1')) $
                                a2'o
                     Just p2' = remPs (xx2patches a1'nop2 n1') p2
                 case (a2', n2', p2') of
                   ([], NilFL, Non c x) | NilFL <- joinEffects c ->
                                          Just (Normal x :> Conflictor a1'nop2 (n1'+>+x:>:NilFL) p1')
                                        | otherwise -> impossible
                   _ -> Just (Conflictor a2' n2' p2' :> Conflictor (p2:a1'nop2) n1' p1')
        where (_,rpn2) = geteff a2 n2
              p1' = addPs (reverseFL rpn2) p1
    -- cHi         -- uses previous match
    commute c@(InvConflictor _ _ _ :> InvConflictor _ _ _) = invertCommute c
    commute _ = Nothing

instance PrimPatch prim => Merge (RealPatch prim) where
    merge (InvConflictor _ _ _ :\/: _) = impossible
    merge (_ :\/: InvConflictor _ _ _) = impossible
    merge (Etacilpud _ :\/: _) = impossible
    merge (_ :\/: Etacilpud _) = impossible
--    merge (x :\/: y) | traceDoc (greenText "merging x" $$ showPatch x $$
--                                 greenText "with y" $$ showPatch y) False = impossible
    -- mA
    merge (Duplicate a :\/: Duplicate b) = Duplicate b :/\: Duplicate a
    -- mB
    merge (Duplicate a :\/: b) = b :/\: Duplicate (addP (invert b) a) -- FIXME ???
    -- smB
    merge m@(_ :\/: Duplicate _) = swapMerge m
    -- mC
--    merge _ | traceDoc (greenText "about to look for conflictingness") False = impossible
    merge (x :\/: y) | Just (y' :> ix') <- commute (invert (assertConsistent x) :> assertConsistent y),
                       Just (y'' :> _) <- commute (x :> y'),
                       IsEq <- y'' =\/= y = --traceDoc (greenText "These didn't conflict") $
                                            assertConsistent y' :/\: invert (assertConsistent ix')
                     | IsEq <- x =\/= y,
                       n <- addP (invert x) $ non x =
                                 --traceDoc (greenText "Found duplicate") $
                                 Duplicate n :/\: Duplicate n
--    merge (x :\/: y) | traceDoc (greenText "trying to merging x" $$ showPatch x $$
--                                 greenText "which conflicts with y" $$ showPatch y) False = impossible
    -- mD
    merge (Normal x :\/: Normal y) =
        Conflictor [] (x:>:NilFL) (non $ Normal y) :/\: Conflictor [] (y:>:NilFL) (non $ Normal x)
    -- mG
    merge (Normal x :\/: Conflictor iy yy y) =
          --traceDoc (greenText "merging Normal x" $$ showPatch x $$
          --          greenText "and Conflictor iy yy y" $$ showPatch (Conflictor iy yy y)) $
          Conflictor iy yyx y :/\: Conflictor (y:iy++nyy) NilFL x'
              where yyx = yy +>+ x:>:NilFL
                    (x':nyy) = reverse $ xx2nons iy yyx
    -- smE+smG
    merge m@(Conflictor _ _ _ :\/: Normal _) = swapMerge m
--    merge (x :\/: y) | traceDoc (greenText "still trying to merge x" $$ showPatch x $$
--                                 greenText "with y" $$ showPatch y) False = impossible
    -- mH see also cH
    merge (Conflictor ix xx x :\/: Conflictor iy yy y) =
        case pullCommonRL (reverseFL xx) (reverseFL yy) of
        CommonRL rxx1 ryy1 c ->
            case commuteRLFL (ryy1 :> invertRL rxx1) of
            Just (ixx' :> ryy') ->
                let xx' = invert ixx'
                    yy' = reverseRL ryy'
                    y':iy' = map (add $ invertFL ixx') (y:iy)
                    x':ix' = map (add ryy') (x:ix)
                    nyy' = xx2nons iy' yy'
                    nxx' = xx2nons ix' xx'
                    icx = drop (lengthRL rxx1) $ xx2nons ix (reverseRL $ c+<+rxx1)
                    ic' = map (add ryy') icx
                    ixy' = ic' ++ (iy'+++ix')
                    -- +++ above is a more efficient version of nub
                    -- (iy'++ix') given that we know each element shows up
                    -- only once in either list.
                in --traceDoc (greenText "here I am! and so is ixy'" $$ showNons ixy' $$
                   --          greenText "and iy" $$ showNons iy $$ greenText (show $ length iy) $$
                   --          greenText "and ix" $$ showNons ix $$
                   --          greenText "and iy'" $$ showNons iy' $$
                   --          greenText "and ix'" $$ showNons ix' $$
                   --          greenText "and ic'" $$ showNons ic'
                   --         ) $
                Conflictor (x':ixy'++nxx') yy' y' :/\: Conflictor (y':ixy'++nyy') xx' x'
            Nothing -> impossible
--    merge _ = error "haven't finished fixing merge"

instance PatchInspect prim => PatchInspect (RealPatch prim) where
    listTouchedFiles (Duplicate p) = nonTouches p
    listTouchedFiles (Etacilpud p) = nonTouches p
    listTouchedFiles (Normal p) = listTouchedFiles p
    listTouchedFiles (Conflictor x c p) =
        nubsort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p
    listTouchedFiles (InvConflictor x c p) =
        nubsort $ concatMap nonTouches x ++ listTouchedFiles c ++ nonTouches p

    hunkMatches f (Duplicate p) = nonHunkMatches f p
    hunkMatches f (Etacilpud p) = nonHunkMatches f p
    hunkMatches f (Normal p) = hunkMatches f p
    hunkMatches f (Conflictor x c p) = or [or $ map (nonHunkMatches f) x, hunkMatches f c, nonHunkMatches f p]
    hunkMatches f (InvConflictor x c p) = or [or $ map (nonHunkMatches f) x, hunkMatches f c, nonHunkMatches f p]

{-
allConflictsWithFL :: FL prim C(x y) -> [Non (RealPatch prim) C(x)]
                     -> ([Non (RealPatch prim) C(x)], [Non (RealPatch prim) C(x)])
allConflictsWithFL xx ns = case partition f ns of
                             ([],nc) -> ([],nc)
                             (c,nc) -> case acw c nc of
                                       (c',nc') -> (c++c',nc')
    where acw (y:ys) zs = case allConflictsWith y zs of
                          (c,nc) -> case acw ys nc of
                                    (c',nc') -> (c++c',nc')
          acw [] zs = ([],zs)
          f (Non c p) = case commuteRLFL (invertFL c :> mapFL_FL Normal xx) of
                        Nothing -> True
                        Just (xx' :> _) -> case commuteFLorComplain (Normal (invert p) :> xx') of
                                           Nothing -> True
                                           Just _ -> False
-}
allConflictsWith :: PrimPatch prim
                   => Non (RealPatch prim) C(x) -> [Non (RealPatch prim) C(x)]
                   -> ([Non (RealPatch prim) C(x)], [Non (RealPatch prim) C(x)])
allConflictsWith x ys = acw $ partition (conflictsWith x) ys
    where acw ([],nc) = ([],nc)
          acw (c:cs, nc) = case allConflictsWith c nc of
                           (c1,nc1) -> case acw (cs, nc1) of
                                       (xs',nc') -> (c:c1++xs',nc')

conflictsWith :: PrimPatch prim => Non (RealPatch prim) C(x) -> Non (RealPatch prim) C(x) -> Bool
conflictsWith x y | x `dependsUpon` y || y `dependsUpon` x = False
conflictsWith x (Non cy y) =
    case remPs cy x of
    Just (Non cx' x') -> case commuteFLorComplain (fromPrim (invert y) :> cx' +>+ fromPrim x' :>: NilFL) of
                         Right _ -> False
                         Left _ -> True
    Nothing -> True

dependsUpon :: PrimPatch prim => Non (RealPatch prim) C(x) -> Non (RealPatch prim) C(x) -> Bool
dependsUpon (Non xs _) (Non ys y) =
    case removeSubsequenceFL (ys +>+ fromPrim y :>: NilFL) xs of
    Just _ -> True
    Nothing -> False

(+++) :: Eq a => [a] -> [a] -> [a]
[] +++ x = x
x +++ [] = x
(x:xs) +++ xys | Just ys <- remove1 x xys = x : (xs +++ ys)
               | otherwise = x : (xs +++ xys)

swapMerge :: PrimPatch prim => (RealPatch prim :\/: RealPatch prim) C(x y) -> (RealPatch prim :/\: RealPatch prim) C(x y)
swapMerge (x :\/: y) = case merge (y :\/: x) of x' :/\: y' -> y' :/\: x'

invertCommute :: PrimPatch prim => (RealPatch prim :> RealPatch prim) C(x y) -> Maybe ((RealPatch prim :> RealPatch prim) C(x y))
invertCommute (x :> y) = do ix' :> iy' <- commute (invert y :> invert x)
                            return (invert iy' :> invert ix')

invertCommuteNC :: PrimPatch prim => (RealPatch prim :> RealPatch prim) C(x y) -> Maybe ((RealPatch prim :> RealPatch prim) C(x y))
invertCommuteNC (x :> y) = do ix' :> iy' <- commuteNoConflicts (invert y :> invert x)
                              return (invert iy' :> invert ix')

-- | 'pullCommon' @xs ys@ returns the set of patches that can be commuted
--   out of both @xs@ and @ys@ along with the remnants of both lists
pullCommon :: Patchy p => FL p C(o x) -> FL p C(o y) -> Common p C(o x y)
pullCommon NilFL ys = Common NilFL NilFL ys
pullCommon xs NilFL = Common NilFL xs NilFL
pullCommon (x:>:xs) xys | Just ys <- removeFL x xys = case pullCommon xs ys of
                                                      Common c xs' ys' -> Common (x:>:c) xs' ys'
pullCommon (x:>:xs) ys = case commuteWhatWeCanFL (x :> xs) of
                         xs1:>x':>xs2 -> case pullCommon xs1 ys of
                                         Common c xs1' ys' -> Common c (xs1'+>+x':>:xs2) ys'

-- | 'Common' @cs xs ys@ represents two sequences of patches that have @cs@ in common,
--   in other words @cs +>+ xs@ and @cs +>+ ys@
data Common p C(o x y) where
    Common :: FL p C(o i) -> FL p C(i x) -> FL p C(i y) -> Common p C(o x y)

-- | 'pullCommonRL' @xs ys@ returns the set of patches that can be commuted
--   out of both @xs@ and @ys@ along with the remnants of both lists
pullCommonRL :: Patchy p => RL p C(x o) -> RL p C(y o) -> CommonRL p C(x y o)
pullCommonRL NilRL ys = CommonRL NilRL ys NilRL
pullCommonRL xs NilRL = CommonRL xs NilRL NilRL
pullCommonRL (x:<:xs) xys
    | Just ys <- removeRL x xys = case pullCommonRL xs ys of
                                  CommonRL xs' ys' c -> CommonRL xs' ys' (x:<:c)
pullCommonRL (x:<:xs) ys =
    case commuteWhatWeCanRL (xs :> x) of
    xs1:>x':>xs2 -> case pullCommonRL xs2 ys of
                    CommonRL xs2' ys' c -> CommonRL (xs2'+<+x':<:xs1) ys' c

-- | 'CommonRL' @xs ys cs@' represents two sequences of patches that have @cs@ in common,
--   in other words @xs +<+ cs@ and @ys +<+ cs@
data CommonRL p C(x y f) where
    CommonRL :: RL p C(x i) -> RL p C(y i) -> RL p C(i f) -> CommonRL p C(x y f)

instance PrimPatch prim => Apply (RealPatch prim) where
    apply p = apply (effect p)

instance PrimPatch prim => RepairToFL (RealPatch prim) where
    applyAndTryToFixFL (Normal p) = mapMaybeSnd (mapFL_FL Normal) `liftM` applyAndTryToFixFL p
    applyAndTryToFixFL x = do apply x; return Nothing

instance PatchListFormat (RealPatch prim) where
   -- In principle we could use ListFormatDefault when prim /= V1 Prim patches,
   -- as those are the only case where we need to support a legacy on-disk
   -- format. In practice we don't expect RealPatch to be used with any other argument
   -- anyway, so it doesn't matter.
    patchListFormat = ListFormatV2

instance PrimPatch prim => ShowPatchBasic (RealPatch prim) where
    showPatch (Duplicate d) = blueText "duplicate" $$ showNon d
    showPatch (Etacilpud d) = blueText "etacilpud" $$ showNon d
    showPatch (Normal p) = showPrim NewFormat p
    showPatch (Conflictor i NilFL p) =
        blueText "conflictor" <+> showNons i <+> blueText "[]" $$ showNon p
    showPatch (Conflictor i cs p) =
        blueText "conflictor" <+> showNons i <+> blueText "[" $$
        showPrimFL NewFormat cs $$
        blueText "]" $$
        showNon p
    showPatch (InvConflictor i NilFL p) =
        blueText "rotcilfnoc" <+> showNons i <+> blueText "[]" $$ showNon p
    showPatch (InvConflictor i cs p) =
        blueText "rotcilfnoc" <+> showNons i <+> blueText "[" $$
        showPrimFL NewFormat cs $$
        blueText "]" $$
        showNon p

instance PrimPatch prim => ShowPatch (RealPatch prim) where
    showContextPatch (Normal p) = showContextPatch p
    showContextPatch c = return $ showPatch c
    summary = plainSummary
    summaryFL = plainSummary
    thing _ = "change"

instance PrimPatch prim => ReadPatch (RealPatch prim) where
 readPatch' = skipSpace >> choice
      [ do string duplicate
           p <- readNon
           return $ Sealed $ Duplicate p
      , do string etacilpud
           p <- readNon
           return $ Sealed $ Etacilpud p
      , do string conflictor
           i <- readNons
           Sealed ps <- bracketedFL (readPrim NewFormat) '[' ']'
           p <- readNon
           return $ Sealed $ Conflictor i (unsafeCoerceP ps) p
      , do string rotcilfnoc
           i <- readNons
           Sealed ps <- bracketedFL (readPrim NewFormat) '[' ']'
           p <- readNon
           return $ Sealed $ InvConflictor i ps p
      , do Sealed p <- readPrim NewFormat
           return $ Sealed $ Normal p
      ]

duplicate :: BC.ByteString
duplicate = BC.pack "duplicate"

etacilpud :: BC.ByteString
etacilpud = BC.pack "etacilpud"

conflictor :: BC.ByteString
conflictor = BC.pack "conflictor"

rotcilfnoc :: BC.ByteString
rotcilfnoc = BC.pack "rotcilfnoc"

instance PrimPatch prim => Show (RealPatch prim C(x y)) where
    show p = renderString $ showPatch p

instance PrimPatch prim => Show2 (RealPatch prim) where
    showDict2 = ShowDictClass

instance PrimPatch prim => Nonable (RealPatch prim) where
    non (Duplicate d) = d
    non (Etacilpud d) = invertNon d -- FIXME !!! ???
    non (Normal p) = Non NilFL p
    non (Conflictor _ xx x) = add (invertFL xx) x
    non (InvConflictor _ _ n) = invertNon n

instance PrimPatch prim => Effect (RealPatch prim) where
    effect (Duplicate _) = NilFL
    effect (Etacilpud _) = NilFL
    effect (Normal p) = p :>: NilFL
    effect (Conflictor _ e _) = invert e
    effect (InvConflictor _ e _) = e
    effectRL (Duplicate _) = NilRL
    effectRL (Etacilpud _) = NilRL
    effectRL (Normal p) = p :<: NilRL
    effectRL (Conflictor _ e _) = invertFL e
    effectRL (InvConflictor _ e _) = reverseFL e

instance IsHunk prim => IsHunk (RealPatch prim) where
    isHunk rp = do Normal p <- return rp
                   isHunk p
