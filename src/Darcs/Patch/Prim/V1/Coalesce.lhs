\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Coalesce
    ()
    where

import Prelude hiding ( pi )
import Data.Map ( elems, fromListWith, mapWithKey )

import qualified Data.ByteString as B (ByteString)

import Darcs.Patch.FileName ( FileName, fp2fn )
import Darcs.Patch.Prim.Class ( PrimCanonize(..) )
import Darcs.Patch.Prim.V1.Commute ()
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..)
    , comparePrim, isIdentity
    )
import Darcs.Patch.Prim.V1.Show ()
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (:<)(..)
                               , reverseRL, mapFL, mapFL_FL
                               , concatFL, lengthFL, (+>+) )
import Darcs.Witnesses.Sealed ( unseal, Sealed2(..), unsafeUnseal2
                              , Gap(..), unFreeLeft
                              )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePEnd )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..) )
-- import Darcs.Patch.Permutations () -- for Invert instance of FL
import Lcs ( getChanges )

#include "gadts.h"
#include "impossible.h"
\end{code}

It can sometimes be handy to have a canonical representation of a given
patch.  We achieve this by defining a canonical form for each patch type,
and a function ``{\tt canonize}'' which takes a patch and puts it into
canonical form.  This routine is used by the diff function to create an
optimal patch (based on an LCS algorithm) from a simple hunk describing the
old and new version of a file.

A simpler, faster (and more generally useful) cousin of canonize is the
coalescing function.  This takes two sequential patches, and tries to turn
them into one patch.  This function is used to deal with ``split'' patches,
which are created when the commutation of a primitive patch can only be
represented by a composite patch.  In this case the resulting composite
patch must return to the original primitive patch when the commutation is
reversed, which a split patch accomplishes by trying to coalesce its
contents each time it is commuted.

\begin{code}
-- | 'coalesce' @p2 :< p1@ tries to combine @p1@ and @p2@ into a single
--   patch without intermediary changes.  For example, two hunk patches
--   modifying adjacent lines can be coalesced into a bigger hunk patch.
--   Or a patch which moves file A to file B can be coalesced with a
--   patch that moves file B into file C, yielding a patch that moves
--   file A to file C.
coalesce :: (Prim :< Prim) C(x y) -> Maybe (FL Prim C(x y))
coalesce (FP f1 _ :< FP f2 _) | f1 /= f2 = Nothing
coalesce (p2 :< p1) | IsEq <- p2 =\/= invert p1 = Just NilFL
coalesce (FP f1 p1 :< FP _ p2) = fmap (:>: NilFL) $ coalesceFilePrim f1 (p1 :< p2) -- f1 = f2
coalesce (Move a b :< Move b' a') | a == a' = Just $ Move b' b :>: NilFL
coalesce (Move a b :< FP f AddFile) | f == a = Just $ FP b AddFile :>: NilFL
coalesce (Move a b :< DP f AddDir) | f == a = Just $ DP b AddDir :>: NilFL
coalesce (FP f RmFile :< Move a b) | b == f = Just $ FP a RmFile :>: NilFL
coalesce (DP f RmDir :< Move a b) | b == f = Just $ DP a RmDir :>: NilFL
coalesce (ChangePref p f1 t1 :< ChangePref p2 f2 t2) | p == p2 && t2 == f1 = Just $ ChangePref p f2 t1 :>: NilFL
coalesce _ = Nothing
\end{code}


\begin{code}
mapPrimFL :: (FORALL(x y) FL Prim C(x y) -> FL Prim C(x y))
             -> FL Prim C(w z) -> FL Prim C(w z)
mapPrimFL f x =
-- an optimisation; break the list up into independent sublists
-- and apply f to each of them
     case mapM toSimpleSealed $ mapFL Sealed2 x of
     Just sx -> concatFL $ unsealList $ elems $
                mapWithKey (\ k p -> Sealed2 (f (fromSimples k (unsealList (p []))))) $
                fromListWith (flip (.)) $
                map (\ (a,b) -> (a,(b:))) sx
     Nothing -> f x
  where
        unsealList :: [Sealed2 p] -> FL p C(a b)
        unsealList [] = unsafeCoerceP NilFL
        unsealList (y:ys) = unsafeUnseal2 y :>: unsealList ys

        toSimpleSealed :: Sealed2 Prim -> Maybe (FileName, Sealed2 Simple)
        toSimpleSealed (Sealed2 p) = fmap (\(fn, s) -> (fn, Sealed2 s)) (toSimple p)



data Simple C(x y) = SFP !(FilePatchType C(x y)) | SDP !(DirPatchType C(x y))
                   | SCP String String String
                   deriving ( Show )

toSimple :: Prim C(x y) -> Maybe (FileName, Simple C(x y))
toSimple (FP a b) = Just (a, SFP b)
toSimple (DP a AddDir) = Just (a, SDP AddDir)
toSimple (DP _ RmDir) = Nothing -- ordering is trickier with rmdir present
toSimple (Move _ _) = Nothing
toSimple (ChangePref a b c) = Just (fp2fn "_darcs/prefs/prefs", SCP a b c)

fromSimple :: FileName -> Simple C(x y) -> Prim C(x y)
fromSimple a (SFP b) = FP a b
fromSimple a (SDP b) = DP a b
fromSimple _ (SCP a b c) = ChangePref a b c

fromSimples :: FileName -> FL Simple C(x y) -> FL Prim C(x y)
fromSimples a bs = mapFL_FL (fromSimple a) bs

tryHarderToShrink :: FL Prim C(x y) -> FL Prim C(x y)
tryHarderToShrink x = tryToShrink2 $ maybe x id (tryShrinkingInverse x)

tryToShrink2 :: FL Prim C(x y) -> FL Prim C(x y)
tryToShrink2 psold =
    let ps = sortCoalesceFL psold
        ps_shrunk = shrinkABit ps
                    in
    if lengthFL ps_shrunk < lengthFL ps
    then tryToShrink2 ps_shrunk
    else ps_shrunk

shrinkABit :: FL Prim C(x y) -> FL Prim C(x y)
shrinkABit NilFL = NilFL
shrinkABit (p:>:ps) =
    case tryOne NilRL p ps of
    Nothing -> p :>: shrinkABit ps
    Just ps' -> ps'

tryOne :: RL Prim C(w x) -> Prim C(x y) -> FL Prim C(y z)
        -> Maybe (FL Prim C(w z))
tryOne _ _ NilFL = Nothing
tryOne sofar p (p1:>:ps) =
    case coalesce (p1 :< p) of
    Just p' -> Just (reverseRL sofar +>+ p' +>+ ps)
    Nothing -> case commute (p :> p1) of
               Nothing -> Nothing
               Just (p1' :> p') -> tryOne (p1':<:sofar) p' ps

-- | The heart of "sortCoalesceFL"
sortCoalesceFL2 :: FL Prim C(x y) -> FL Prim C(x y)
sortCoalesceFL2 NilFL = NilFL
sortCoalesceFL2 (x:>:xs) | IsEq <- isIdentity x = sortCoalesceFL2 xs
sortCoalesceFL2 (x:>:xs) = either id id $ pushCoalescePatch x $ sortCoalesceFL2 xs

-- | 'pushCoalescePatch' @new ps@ is almost like @new :>: ps@ except
--   as an alternative to consing, we first try to coalesce @new@ with
--   the head of @ps@.  If this fails, we try again, using commutation
--   to push @new@ down the list until we find a place where either
--   (a) @new@ is @LT@ the next member of the list [see 'comparePrim']
--   (b) commutation fails or
--   (c) coalescing succeeds.
--   The basic principle is to coalesce if we can and cons otherwise.
--
--   As an additional optimization, pushCoalescePatch outputs a Left
--   value if it wasn't able to shrink the patch sequence at all, and
--   a Right value if it was indeed able to shrink the patch sequence.
--   This avoids the O(N) calls to lengthFL that were in the older
--   code.
--
--   Also note that pushCoalescePatch is only ever used (and should
--   only ever be used) as an internal function in in
--   sortCoalesceFL2.
pushCoalescePatch :: Prim C(x y) -> FL Prim C(y z)
                    -> Either (FL Prim C(x z)) (FL Prim C(x z))
pushCoalescePatch new NilFL = Left (new:>:NilFL)
pushCoalescePatch new ps@(p:>:ps')
    = case coalesce (p :< new) of
      Just (new' :>: NilFL) -> Right $ either id id $ pushCoalescePatch new' ps'
      Just NilFL -> Right ps'
      Just _ -> impossible -- coalesce either returns a singleton or empty
      Nothing -> if comparePrim new p == LT then Left (new:>:ps)
                            else case commute (new :> p) of
                                 Just (p' :> new') ->
                                     case pushCoalescePatch new' ps' of
                                     Right r -> Right $ either id id $
                                                pushCoalescePatch p' r
                                     Left r -> Left (p' :>: r)
                                 Nothing -> Left (new:>:ps)

coalesceFilePrim :: FileName -> (FilePatchType :< FilePatchType) C(x y)
                  -> Maybe (Prim C(x y))
coalesceFilePrim f (Hunk line1 old1 new1 :< Hunk line2 old2 new2)
    = coalesceHunk f line1 old1 new1 line2 old2 new2
-- Token replace patches operating right after (or before) AddFile (RmFile)
-- is an identity patch, as far as coalescing is concerned.
coalesceFilePrim f (TokReplace _ _ _ :< AddFile) = Just $ FP f AddFile
coalesceFilePrim f (RmFile :< TokReplace _ _ _) = Just $ FP f RmFile
coalesceFilePrim f (TokReplace t1 o1 n1 :< TokReplace t2 o2 n2)
    | t1 == t2 && n2 == o1 = Just $ FP f $ TokReplace t1 o2 n1
coalesceFilePrim f (Binary m n :< Binary o m')
    | m == m' = Just $ FP f $ Binary o n
coalesceFilePrim _ _ = Nothing

\end{code}
Hunks, of course, can be coalesced if they have any overlap.  Note that
coalesce code doesn't check if the two patches are conflicting.  If you are
coalescing two conflicting hunks, you've already got a bug somewhere.

\begin{code}
coalesceHunk :: FileName
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Int -> [B.ByteString] -> [B.ByteString]
             -> Maybe (Prim C(x y))
coalesceHunk f line1 old1 new1 line2 old2 new2
    | line1 == line2 && lengthold1 < lengthnew2 =
        if take lengthold1 new2 /= old1
        then Nothing
        else case drop lengthold1 new2 of
        extranew -> Just (FP f (Hunk line1 old2 (new1 ++ extranew)))
    | line1 == line2 && lengthold1 > lengthnew2 =
        if take lengthnew2 old1 /= new2
        then Nothing
        else case drop lengthnew2 old1 of
        extraold -> Just (FP f (Hunk line1 (old2 ++ extraold) new1))
    | line1 == line2 = if new2 == old1 then Just (FP f (Hunk line1 old2 new1))
                       else Nothing
    | line1 < line2 && lengthold1 >= line2 - line1 =
        case take (line2 - line1) old1 of
        extra-> coalesceHunk f line1 old1 new1 line1 (extra ++ old2) (extra ++ new2)
    | line1 > line2 && lengthnew2 >= line1 - line2 =
        case take (line1 - line2) new2 of
        extra-> coalesceHunk f line2 (extra ++ old1) (extra ++ new1) line2 old2 new2
    | otherwise = Nothing
    where lengthold1 = length old1
          lengthnew2 = length new2
\end{code}

One of the most important pieces of code is the canonization of a hunk,
which is where the ``diff'' algorithm is performed.  This algorithm begins
with chopping off the identical beginnings and endings of the old and new
hunks.  This isn't strictly necessary, but is a good idea, since this
process is $O(n)$, while the primary diff algorithm is something
considerably more painful than that\ldots\ actually the head would be dealt
with all right, but with more space complexity.  I think it's more
efficient to just chop the head and tail off first.

\begin{code}
canonizeHunk :: Gap w
             => FileName -> Int -> [B.ByteString] -> [B.ByteString]
             -> w (FL Prim)
canonizeHunk f line old new
    | null old || null new
        = freeGap (FP f (Hunk line old new) :>: NilFL)
canonizeHunk f line old new = makeHoley f line $ getChanges old new

makeHoley :: Gap w
          => FileName -> Int -> [(Int,[B.ByteString], [B.ByteString])]
          -> w (FL Prim)
makeHoley f line changes =
    foldr (joinGap (:>:)) (emptyGap NilFL) $
    map (\ (l,o,n) -> freeGap (FP f (Hunk (l+line) o n))) changes
\end{code}

\begin{code}
instance PrimCanonize Prim where
   tryToShrink = mapPrimFL tryHarderToShrink

   tryShrinkingInverse (x:>:y:>:z)
       | IsEq <- invert x =\/= y = Just z
       | otherwise = case tryShrinkingInverse (y:>:z) of
                     Nothing -> Nothing
                     Just yz' -> Just $ case tryShrinkingInverse (x:>:yz') of
                                        Nothing -> x:>:yz'
                                        Just xyz' -> xyz'
   tryShrinkingInverse _ = Nothing

   sortCoalesceFL = mapPrimFL sortCoalesceFL2
   canonize p | IsEq <- isIdentity p = NilFL
   canonize (FP f (Hunk line old new)) = unseal unsafeCoercePEnd $ unFreeLeft $ canonizeHunk f line old new
   canonize p = p :>: NilFL
   -- Running canonize twice is apparently necessary to fix issue525;
   -- would be nice to understand why.
   canonizeFL = concatFL . mapFL_FL canonize . sortCoalesceFL .
                concatFL . mapFL_FL canonize
   join (x :> y) = coalesce (y :< x)

\end{code}