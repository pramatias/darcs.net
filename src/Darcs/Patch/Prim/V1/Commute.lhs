\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Commute
    ( Perhaps(..)
    , subcommutes, WrappedCommuteFunction(..)
    )
    where

import Prelude hiding ( pi )
import Control.Monad ( MonadPlus, msum, mzero, mplus )

import qualified Data.ByteString as B (ByteString, concat)
import qualified Data.ByteString.Char8 as BC (pack)

import Darcs.Patch.FileName ( FileName, fn2fp, movedirfilename )
import Darcs.Witnesses.Ordered ( (:<)(..) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Patch.Prim.Class ( is_filepatch )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Commute ( Commute(..), toFwdCommute, toRevCommute )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.TokenReplace ( tryTokInternal )

#include "impossible.h"
#include "gadts.h"

isInDirectory :: FileName -> FileName -> Bool
isInDirectory d f = iid (fn2fp d) (fn2fp f)
    where iid (cd:cds) (cf:cfs)
              | cd /= cf = False
              | otherwise = iid cds cfs
          iid [] ('/':_) = True
          iid [] [] = True -- Count directory itself as being in directory...
          iid _ _ = False

data Perhaps a = Unknown | Failed | Succeeded a

instance  Monad Perhaps where
    (Succeeded x) >>= k =  k x
    Failed   >>= _      =  Failed
    Unknown  >>= _      =  Unknown
    Failed   >> _       =  Failed
    (Succeeded _) >> k  =  k
    Unknown  >> k       =  k
    return              =  Succeeded
    fail _              =  Unknown

instance  MonadPlus Perhaps where
    mzero                 = Unknown
    Unknown `mplus` ys    = ys
    Failed  `mplus` _     = Failed
    (Succeeded x) `mplus` _ = Succeeded x

toMaybe :: Perhaps a -> Maybe a
toMaybe (Succeeded x) = Just x
toMaybe _ = Nothing

toPerhaps :: Maybe a -> Perhaps a
toPerhaps (Just x) = Succeeded x
toPerhaps Nothing = Failed

cleverCommute :: CommuteFunction -> CommuteFunction
cleverCommute c (p1:<p2) =
    case c (p1 :< p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed
    Unknown -> case c (invert p2 :< invert p1) of
               Succeeded (p1' :< p2') -> Succeeded (invert p2' :< invert p1')
               Failed -> Failed
               Unknown -> Unknown
--cleverCommute c (p1,p2) = c (p1,p2) `mplus`
--    (case c (invert p2,invert p1) of
--     Succeeded (p1', p2') -> Succeeded (invert p2', invert p1')
--     Failed -> Failed
--     Unknown -> Unknown)

speedyCommute :: CommuteFunction  -- Deal with common cases quickly!
    -- Two file-patches modifying different files trivially commute.
speedyCommute (p2@(FP f' _) :< p1@(FP f _)) 
  | f /= f' = Succeeded (unsafeCoerceP p1 :< unsafeCoerceP p2)
speedyCommute _other = Unknown

everythingElseCommute :: CommuteFunction
everythingElseCommute x = eec x
    where
    eec :: CommuteFunction
    eec (ChangePref p f t :<p1) = Succeeded (unsafeCoerceP p1 :< ChangePref p f t)
    eec (p2 :<ChangePref p f t) = Succeeded (ChangePref p f t :< unsafeCoerceP p2)
    eec xx = cleverCommute commuteFiledir                xx

{-
Note that it must be true that

commutex (A^-1 A, P) = Just (P, A'^-1 A')

and

if commutex (A, B) == Just (B', A')
then commutex (B^-1, A^-1) == Just (A'^-1, B'^-1)
-}

instance Commute Prim where
    commute x = toMaybe $ msum [toFwdCommute speedyCommute x,
                                toFwdCommute everythingElseCommute x
                               ]

isSuperdir :: FileName -> FileName -> Bool
isSuperdir d1 d2 = isd (fn2fp d1) (fn2fp d2)
    where isd s1 s2 =
              length s2 >= length s1 + 1 && take (length s1 + 1) s2 == s1 ++ "/"

commuteFiledir :: CommuteFunction
commuteFiledir (FP f1 p1 :< FP f2 p2) =
  if f1 /= f2 then Succeeded ( FP f2 (unsafeCoerceP p2) :< FP f1 (unsafeCoerceP p1) )
  else commuteFP f1 (p1 :< p2)
commuteFiledir (DP d1 p1 :< DP d2 p2) =
  if (not $ isInDirectory d1 d2) && (not $ isInDirectory d2 d1) &&
     d1 /= d2
  then Succeeded ( DP d2 (unsafeCoerceP p2) :< DP d1 (unsafeCoerceP p1) )
  else Failed
commuteFiledir (DP d dp :< FP f fp) =
    if not $ isInDirectory d f
    then Succeeded (FP f (unsafeCoerceP fp) :< DP d (unsafeCoerceP dp))
    else Failed

commuteFiledir (Move d d' :< FP f2 p2)
    | f2 == d' = Failed
    | (p2 == AddFile || p2 == RmFile) && d == f2 = Failed
    | otherwise = Succeeded (FP (movedirfilename d d' f2) (unsafeCoerceP p2) :< Move d d')
commuteFiledir (Move d d' :< DP d2 p2)
    | isSuperdir d2 d' || isSuperdir d2 d = Failed
    | d == d2 = Failed  -- 'p2 == AddDir' is omitted since it is unuseful,
                        -- but note that if p2 == RmDir then patches would not be sequential.
    | d2 == d' = Failed
    | otherwise = Succeeded (DP (movedirfilename d d' d2) (unsafeCoerceP p2) :< Move d d')
commuteFiledir (Move d d' :< Move f f')
    | f == d' || f' == d = Failed
    | f == d || f' == d' = Failed
    | d `isSuperdir` f && f' `isSuperdir` d' = Failed
    | otherwise =
        Succeeded (Move (movedirfilename d d' f) (movedirfilename d d' f') :<
                   Move (movedirfilename f' f d) (movedirfilename f' f d'))

commuteFiledir _ = Unknown

type CommuteFunction = FORALL(x y) (Prim :< Prim) C(x y) -> Perhaps ((Prim :< Prim) C(x y))
newtype WrappedCommuteFunction = WrappedCommuteFunction { runWrappedCommuteFunction :: CommuteFunction }

subcommutes :: [(String, WrappedCommuteFunction)]
subcommutes =
    [("speedyCommute", WrappedCommuteFunction speedyCommute),
     ("commuteFiledir", WrappedCommuteFunction (cleverCommute commuteFiledir)),
     ("commuteFilepatches", WrappedCommuteFunction (cleverCommute commuteFilepatches)),
     ("commutex", WrappedCommuteFunction (toPerhaps . toRevCommute commute))
    ]
\end{code}

\subsection{File patches}

A file patch is a patch which only modifies a single
file.  There are some rules which can be made about file patches in
general, which makes them a handy class.
For example, commutation of two filepatches is trivial if they modify
different files.  If they happen to
modify the same file, we'll have to check whether or not they commutex.
\begin{code}
commuteFilepatches :: CommuteFunction
commuteFilepatches (FP f1 p1 :< FP f2 p2) | f1 == f2 = commuteFP f1 (p1 :< p2)
commuteFilepatches _ = Unknown

commuteFP :: FileName -> (FilePatchType :< FilePatchType) C(x y)
          -> Perhaps ((Prim :< Prim) C(x y))
commuteFP f (Hunk line1 [] [] :< p2) =
    seq f $ Succeeded (FP f (unsafeCoerceP p2) :< FP f (Hunk line1 [] []))
commuteFP f (p2 :< Hunk line1 [] []) =
    seq f $ Succeeded (FP f (Hunk line1 [] []) :< FP f (unsafeCoerceP p2))
commuteFP f (Hunk line1 old1 new1 :< Hunk line2 old2 new2) = seq f $
  toPerhaps $ commuteHunk f (Hunk line1 old1 new1 :< Hunk line2 old2 new2)
commuteFP f (TokReplace t o n :< Hunk line2 old2 new2) = seq f $
    case tryTokReplace t o n old2 of
    Nothing -> Failed
    Just old2' ->
      case tryTokReplace t o n new2 of
      Nothing -> Failed
      Just new2' -> Succeeded (FP f (Hunk line2 old2' new2') :<
                               FP f (TokReplace t o n))
commuteFP f (TokReplace t o n :< TokReplace t2 o2 n2)
    | seq f $ t /= t2 = Failed
    | o == o2 = Failed
    | n == o2 = Failed
    | o == n2 = Failed
    | n == n2 = Failed
    | otherwise = Succeeded (FP f (TokReplace t2 o2 n2) :<
                             FP f (TokReplace t o n))
commuteFP _ _ = Unknown

\end{code}

\subsection{Hunks}

The hunk is the simplest patch that has a commuting pattern in which the
commuted patches differ from the originals (rather than simple success or
failure).  This makes commuting or merging two hunks a tad tedious.
\begin{code}
commuteHunk :: FileName -> (FilePatchType :< FilePatchType) C(x y)
            -> Maybe ((Prim :< Prim) C(x y))
commuteHunk f (Hunk line2 old2 new2 :< Hunk line1 old1 new1)
  | seq f $ line1 + lengthnew1 < line2 =
      Just (FP f (Hunk line1 old1 new1) :<
            FP f (Hunk (line2 - lengthnew1 + lengthold1) old2 new2))
  | line2 + lengthold2 < line1 =
      Just (FP f (Hunk (line1+ lengthnew2 - lengthold2) old1 new1) :<
            FP f (Hunk line2 old2 new2))
  | line1 + lengthnew1 == line2 &&
    lengthold2 /= 0 && lengthold1 /= 0 && lengthnew2 /= 0 && lengthnew1 /= 0 =
      Just (FP f (Hunk line1 old1 new1) :<
            FP f (Hunk (line2 - lengthnew1 + lengthold1) old2 new2))
  | line2 + lengthold2 == line1 &&
    lengthold2 /= 0 && lengthold1 /= 0 && lengthnew2 /= 0 && lengthnew1 /= 0 =
      Just (FP f (Hunk (line1 + lengthnew2 - lengthold2) old1 new1) :<
            FP f (Hunk line2 old2 new2))
  | otherwise = seq f Nothing
  where lengthnew1 = length new1
        lengthnew2 = length new2
        lengthold1 = length old1
        lengthold2 = length old2
commuteHunk _ _ = impossible
\end{code}

\begin{code}

tryTokReplace :: String -> String -> String
                -> [B.ByteString] -> Maybe [B.ByteString]
tryTokReplace t o n mss =
  mapM (fmap B.concat . tryTokInternal t (BC.pack o) (BC.pack n)) mss

\end{code}