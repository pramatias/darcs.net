%  Copyright (C) 2002-2003 David Roundy
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; see the file COPYING.  If not, write to
%  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%  Boston, MA 02110-1301, USA.


\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.V1.Commute
    (
      merge,
      merger, unravel,
      publicUnravel,
    )
       where

import Control.Monad ( MonadPlus, mplus, msum, mzero, guard )

import Darcs.Patch.Commute ( toFwdCommute )
import Darcs.Patch.ConflictMarking ( mangleUnravelled )
import Darcs.Patch.FileName ( FileName )
import Darcs.Patch.Invert ( invertRL )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Commute(..), PatchInspect(..), Invert(..) )
import Darcs.Patch.V1.Core ( Patch(..),
                             isMerger,
                             mergerUndo )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts(..) )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Prim ( FromPrim(..), PrimPatch,
                          is_filepatch, sortCoalesceFL,
                        )
import Darcs.Patch.Permutations ( headPermutationsRL, simpleHeadPermutationsFL,
                                  commuterIdFL, commuterFLId, selfCommuter )
import Printer ( text, vcat, ($$) )
import Darcs.Patch.V1.Show ( showPatch_ )
import Data.List ( nub, nubBy )
import Darcs.Witnesses.Sealed ( unsafeUnseal, unsafeUnsealFlipped )
import Darcs.Utils ( nubsort )
#include "impossible.h"
import Darcs.Witnesses.Sealed ( Sealed(..), mapSeal, unseal, FlippedSeal(..), mapFlipped )
import Darcs.Witnesses.Eq ( EqCheck(..), MyEq(..) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart
                              , unsafeCoercePEnd )
import Darcs.Witnesses.Ordered ( mapFL_FL,
                             FL(..), RL(..),
                             (:/\:)(..), (:<)(..), (:\/:)(..), (:>)(..),
                             lengthFL, mapRL,
                             reverseFL, reverseRL, concatFL
                           )

--import Darcs.ColorPrinter ( traceDoc )
--import Printer ( greenText )
\end{code}

\section{Commuting patches}

\subsection{Composite patches}

Composite patches are made up of a series of patches intended to be applied
sequentially.  They are represented by a list of patches, with the first
patch in the list being applied first.

\newcommand{\commutex}{\longleftrightarrow}
\newcommand{\commutes}{\longleftrightarrow}

The first way (of only two) to change the context of a patch is by
commutation, which is the process of changing the order of two sequential
patches.
\begin{dfn}
The commutation of patches $P_1$ and $P_2$ is represented by
\[ P_2 P_1 \commutes {P_1}' {P_2}'. \]
Here $P_1'$ is intended to describe the same change as $P_1$, with the
only difference being that $P_1'$ is applied after $P_2'$ rather than
before $P_2$.
\end{dfn}
The above definition is obviously rather vague, the reason being that what
is the ``same change'' has not been defined, and we simply assume (and
hope) that the code's view of what is the ``same change'' will match those
of its human users.  The `$\commutes$' operator should be read as something
like the $==$ operator in C, indicating that the right hand side performs
identical changes to the left hand side, but the two patches are in
reversed order.  When read in this manner, it is clear that commutation
must be a reversible process, and indeed this means that commutation
\emph{can} fail, and must fail in certain cases.  For example, the creation
and deletion of the same file cannot be commuted.  When two patches fail to
commutex, it is said that the second patch depends on the first, meaning
that it must have the first patch in its context (remembering that the
context of a patch is a set of patches, which is how we represent a tree).
\footnote{The fact that commutation can fail makes a huge difference in the
whole patch formalism.  It may be possible to create a formalism in which
commutation always succeeds, with the result of what would otherwise be a
commutation that fails being something like a virtual particle (which can
violate conservation of energy), and it may be that such a formalism would
allow strict mathematical proofs (whereas those used in the current
formalism are mostly only hand waving ``physicist'' proofs).  However, I'm
not sure how you'd deal with a request to delete a file that has not yet
been created, for example.  Obviously you'd need to create some kind of
antifile, which would annihilate with the file when that file finally got
created, but I'm not entirely sure how I'd go about doing this.
$\ddot\frown$ So I'm sticking with my hand waving formalism.}

%I should add that one using the inversion relationship of sequential
%patches, one can avoid having to provide redundant definitions of
%commutation.

% There is another interesting property which is that a commutex's results
% can't be affected by commuting another thingamabopper.

\begin{code}
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

cleverCommute :: Invert prim => CommuteFunction prim -> CommuteFunction prim
cleverCommute c (p1:<p2) =
    case c (p1 :< p2) of
    Succeeded x -> Succeeded x
    Failed -> Failed
    Unknown -> case c (invert p2 :< invert p1) of
               Succeeded (p1' :< p2') -> Succeeded (invert p2' :< invert p1')
               Failed -> Failed
               Unknown -> Unknown

speedyCommute :: PrimPatch prim => CommuteFunction prim
speedyCommute (p1 :< p2) -- Deal with common case quickly!
    | p1_modifies /= Nothing && p2_modifies /= Nothing &&
      p1_modifies /= p2_modifies = Succeeded (unsafeCoerceP p2 :< unsafeCoerceP p1)
    | otherwise = Unknown
    where p1_modifies = isFilepatchMerger p1
          p2_modifies = isFilepatchMerger p2

everythingElseCommute :: forall prim . PrimPatch prim => CommuteFunction prim
everythingElseCommute x = eec x
    where
    eec :: CommuteFunction prim
    eec (PP px :< PP py) = toPerhaps $ do x' :> y' <- commute (py :> px)
                                          return (PP y' :< PP x')
    eec _xx =
        msum [
              cleverCommute commuteRecursiveMerger       _xx
             ,cleverCommute otherCommuteRecursiveMerger _xx
             ]

{-
Note that it must be true that

commutex (A^-1 A, P) = Just (P, A'^-1 A')

and

if commutex (A, B) == Just (B', A')
then commutex (B^-1, A^-1) == Just (A'^-1, B'^-1)
-}

unsafeMerger :: PrimPatch prim => String -> Patch prim C(x y) -> Patch prim C(x z) -> Patch prim C(a b)
unsafeMerger x p1 p2 = unsafeCoercePStart $ unsafeUnseal $ merger x p1 p2

mergerCommute :: PrimPatch prim => (Patch prim :< Patch prim) C(x y) -> Perhaps ((Patch prim :< Patch prim) C(x y))
mergerCommute (Merger _ _ p1 p2 :< pA)
    | unsafeCompare pA p1 = Succeeded (unsafeMerger "0.0" p2 p1 :< unsafeCoercePStart p2)
    | unsafeCompare pA (invert (unsafeMerger "0.0" p2 p1)) = Failed
mergerCommute (Merger _ _
                (Merger _ _ c b)
                (Merger _ _ c' a) :<
                Merger _ _ b' c'')
    | unsafeCompare b' b && unsafeCompare c c' && unsafeCompare c c'' =
        Succeeded (unsafeMerger "0.0" (unsafeMerger "0.0" b (unsafeCoercePStart a)) (unsafeMerger "0.0" b c) :<
                   unsafeMerger "0.0" b (unsafeCoercePStart a))
mergerCommute _ = Unknown

instance PrimPatch prim => Merge (Patch prim) where
    merge (y :\/: z) =
        case actualMerge (y:\/:z) of
        Sealed y' -> case commute (z :> y') of
                         Nothing -> bugDoc $ text "merge_patches bug"
                                    $$ showPatch_ y
                                   $$ showPatch_ z
                                   $$ showPatch_ y'
                         Just (_ :> z') -> -- actualMerge returns one arm of a
                                           -- merge result, so commuting then gives
                                           -- us the other arm but we have to assert
                                           -- that the starting context is correct
                                           unsafeCoercePStart z' :/\: y'

instance PrimPatch prim => Commute (Patch prim) where
    commute x = toMaybe $ msum
                  [toFwdCommute speedyCommute x,
                   toFwdCommute (cleverCommute mergerCommute) x,
                   toFwdCommute everythingElseCommute x
                  ]

instance PrimPatch prim => PatchInspect (Patch prim) where
    -- Recurse on everything, these are potentially spoofed patches
    listTouchedFiles (Merger _ _ p1 p2) = nubsort $ listTouchedFiles p1
                                            ++ listTouchedFiles p2
    listTouchedFiles c@(Regrem _ _ _ _) = listTouchedFiles $ invert c
    listTouchedFiles (PP p) = listTouchedFiles p

    hunkMatches f (Merger _ _ p1 p2) = hunkMatches f p1 || hunkMatches f p2
    hunkMatches f c@(Regrem _ _ _ _) = hunkMatches f $ invert c
    hunkMatches f (PP p) = hunkMatches f p

commuteNoMerger :: PrimPatch prim => MaybeCommute prim
commuteNoMerger x =
    toMaybe $ msum [speedyCommute x,
                    everythingElseCommute x]

isFilepatchMerger :: PrimPatch prim => Patch prim C(x y) -> Maybe FileName
isFilepatchMerger (PP p) = is_filepatch p
isFilepatchMerger (Merger _ _ p1 p2) = do
     f1 <- isFilepatchMerger p1
     f2 <- isFilepatchMerger p2
     if f1 == f2 then return f1 else Nothing
isFilepatchMerger (Regrem und unw p1 p2)
    = isFilepatchMerger (Merger und unw p1 p2)

commuteRecursiveMerger :: PrimPatch prim => (Patch prim :< Patch prim) C(x y) -> Perhaps ((Patch prim :< Patch prim) C(x y))
commuteRecursiveMerger (p@(Merger _ _ p1 p2) :< pA) = toPerhaps $
  do (_ :> pA') <- commuterIdFL selfCommuter (pA :> undo)
     _ <- commuterIdFL selfCommuter (pA' :> invert undo)
     (_ :> pAmid) <- commute (pA :> unsafeCoercePStart (invert p1))
     (p1' :> pAx) <- commute (pAmid :> p1)
     guard (pAx `unsafeCompare` pA)
     (p2' :> _) <- commute (pAmid :> p2)
     (p2o :> _) <- commute (invert pAmid :> p2')
     guard (p2o `unsafeCompare` p2)
     let p' = if unsafeCompare p1' p1 && unsafeCompare p2' p2
              then unsafeCoerceP p
              else unsafeMerger "0.0" p1' p2'
         undo' = mergerUndo p'
     (pAo :> _) <- commuterFLId selfCommuter (undo' :> pA')
     guard (pAo `unsafeCompare` pA)
     return (pA' :< p')
    where undo = mergerUndo p
commuteRecursiveMerger _ = Unknown

otherCommuteRecursiveMerger :: PrimPatch prim => (Patch prim :< Patch prim) C(x y) -> Perhaps ((Patch prim :< Patch prim) C(x y))
otherCommuteRecursiveMerger (pA':< p_old@(Merger _ _ p1' p2')) =
  toPerhaps $
  do (pA :> _) <- commuterFLId selfCommuter (mergerUndo p_old :> pA')
     (pAmid :> p1) <- commute (unsafeCoercePEnd p1' :> pA)
     (_ :> pAmido) <- commute (pA :> invert p1)
     guard (pAmido `unsafeCompare` pAmid)
     (p2 :> _) <- commute (invert pAmid :> p2')
     (p2o' :> _) <- commute (pAmid :> p2)
     guard (p2o' `unsafeCompare` p2')
     let p = if p1 `unsafeCompare` p1' && p2 `unsafeCompare` p2'
             then unsafeCoerceP p_old
             else unsafeMerger "0.0" p1 p2
         undo = mergerUndo p
     guard (not $ pA `unsafeCompare` p1) -- special case here...
     (_ :> pAo') <- commuterIdFL selfCommuter (pA :> undo)
     guard (pAo' `unsafeCompare` pA')
     return (p :< pA)
otherCommuteRecursiveMerger _ = Unknown

type CommuteFunction prim = FORALL(x y) (Patch prim :< Patch prim) C(x y) -> Perhaps ((Patch prim :< Patch prim) C(x y))
type MaybeCommute prim = FORALL(x y) (Patch prim :< Patch prim) C(x y) -> Maybe ((Patch prim :< Patch prim) C(x y))

revCommuteFLId :: MaybeCommute prim -> (FL (Patch prim) :< Patch prim) C(x y) -> Maybe ((Patch prim :< FL (Patch prim)) C(x y))
revCommuteFLId _        (NilFL :< p) = return (p :< NilFL)
revCommuteFLId commuter ((q :>: qs) :< p) = do
   p' :< q' <- commuter (q :< p)
   p'' :< qs' <- revCommuteFLId commuter (qs :< p')
   return (p'' :< (q' :>: qs'))

\end{code}

\paragraph{Merge}
\newcommand{\merge}{\Longrightarrow}
The second way one can change the context of a patch is by a {\bf merge}
operation.  A merge is an operation that takes two parallel patches and
gives a pair of sequential patches.  The merge operation is represented by
the arrow ``\( \merge \)''.
\begin{dfn}\label{merge_dfn}
The result of a merge of two patches, $P_1$ and $P_2$ is one of two patches,
$P_1'$ and $P_2'$, which satisfy the relationship:
\[  P_2 \parallel P_1 \merge {P_2}' P_1 \commutex {P_1}' P_2. \]
\end{dfn}
Note that the sequential patches resulting from a merge are \emph{required}
to commutex.  This is an important consideration, as without it most of the
manipulations we would like to perform would not be possible.  The other
important fact is that a merge \emph{cannot fail}.  Naively, those two
requirements seem contradictory.  In reality, what it means is that the
result of a merge may be a patch which is much more complex than any we
have yet considered\footnote{Alas, I don't know how to prove that the two
constraints even \emph{can} be satisfied.  The best I have been able to do
is to believe that they can be satisfied, and to be unable to find an case
in which my implementation fails to satisfy them.  These two requirements
are the foundation of the entire theory of patches (have you been counting
how many foundations it has?).}.

\subsection{How merges are actually performed}

The constraint that any two compatible patches (patches which can
successfully be applied to the same tree) can be merged is actually quite
difficult to apply.  The above merge constraints also imply that the result
of a series of merges must be independent of the order of the merges.  So
I'm putting a whole section here for the interested to see what algorithms
I use to actually perform the merges (as this is pretty close to being the
most difficult part of the code).

The first case is that in which the two merges don't actually conflict, but
don't trivially merge either (e.g.\ hunk patches on the same file, where the
line number has to be shifted as they are merged).  This kind of merge can
actually be very elegantly dealt with using only commutation and inversion.

There is a handy little theorem which is immensely useful when trying to
merge two patches.
\begin{thm}\label{merge_thm}
$ P_2' P_1 \commutex P_1' P_2 $ if and only if $ P_1'^{ -1}
P_2' \commutex P_2 P_1^{ -1} $, provided both commutations succeed.  If
either commutex fails, this theorem does not apply.
\end{thm}
This can easily be proven by multiplying both sides of the first
commutation by $P_1'^{ -1}$ on the left, and by $P_1^{ -1}$ on the right.
Besides being used in merging, this theorem is also useful in the recursive
commutations of mergers.  From Theorem~\ref{merge_thm}, we see that the
merge of $P_1$ and $P_2'$ is simply the commutation of $P_2$ with $P_1^{
-1}$ (making sure to do the commutation the right way).  Of course, if this
commutation fails, the patches conflict.  Moreover, one must check that the
merged result actually commutes with $P_1$, as the theorem applies only
when \emph{both} commutations are successful.

\begin{code}

elegantMerge :: PrimPatch prim
              => (Patch prim :\/: Patch prim) C(x y)
              -> Maybe ((Patch prim :/\: Patch prim) C(x y))
elegantMerge (p1 :\/: p2) = do
  p1' :> ip2' <- commute (invert p2 :> p1)
  p1o :> _    <- commute (p2 :> p1')
  guard $ unsafeCompare p1o p1 -- should be a redundant check
  return $ invert ip2' :/\: p1'

\end{code}

Of course, there are patches that actually conflict, meaning a merge where
the two patches truly cannot both be applied (e.g.\ trying to create a file
and a directory with the same name).  We deal with this case by creating a
special kind of patch to support the merge, which we will call a
``merger''.  Basically, a merger is a patch that contains the two patches
that conflicted, and instructs darcs basically to resolve the conflict.  By
construction a merger will satisfy the commutation property (see
Definition~\ref{merge_dfn}) that characterizes all merges.  Moreover the
merger's properties are what makes the order of merges unimportant (which
is a rather critical property for darcs as a whole).

The job of a merger is basically to undo the two conflicting patches, and
then apply some sort of a ``resolution'' of the two instead.  In the case
of two conflicting hunks, this will look much like what CVS does, where it
inserts both versions into the file.  In general, of course, the two
conflicting patches may both be mergers themselves, in which case the
situation is considerably more complicated.

\begin{code}
{-
A note about mergers and type witnesses
---------------------------------------

The merger code predates the introduction of type witnesses, and because
of its complexity has proved the hardest part of the codebase to retrofit.
Attempting to do this has exposed various places where the code behaves
oddly (e.g. 'putBefore' below); these are likely to be bugs but fixing
them would be potentially disruptive and dangerous as it might change
the existing merge behaviour and thus break existing repositories.

As a result the addition of witnesses to this code has required the
liberal use of unsafe operators. In effect, witnesses bring no safety
in this area; the sole purpose of adding them here was to allow this
code to run as part of a codebase that uses witnesses everywhere else.

A key problem point is the type of the 'Merger' and 'Regrem' constructors
of Patch, where the witnesses seem odd. It is likely that some or many
of the unsafe operations could be removed by finding a better type for
these constructors.
-}


actualMerge :: PrimPatch prim => (Patch prim :\/: Patch prim) C(x y) -> Sealed (Patch prim C(y))

actualMerge (p1 :\/: p2) = case elegantMerge (p1:\/:p2) of
                             Just (_ :/\: p1') -> Sealed p1'
                             Nothing -> merger "0.0" p2 p1

\end{code}

Much of the merger code depends on a routine which recreates from a single
merger the entire sequence of patches which led up to that merger (this is,
of course, assuming that this is the complicated general case of a merger
of mergers of mergers).  This ``unwind'' procedure is rather complicated,
but absolutely critical to the merger code, as without it we wouldn't even
be able to undo the effects of the patches involved in the merger, since we
wouldn't know what patches were all involved in it.

Basically, unwind takes a merger such as
\begin{verbatim}
M( M(A,B), M(A,M(C,D)))
\end{verbatim}
From which it recreates a merge history:
\begin{verbatim}
C
A
M(A,B)
M( M(A,B), M(A,M(C,D)))
\end{verbatim}
(For the curious, yes I can easily unwind this merger in my head [and on
paper can unwind insanely more complex mergers]---that's what comes of
working for a few months on an algorithm.)  Let's start with a simple
unwinding.  The merger \verb!M(A,B)! simply means that two patches
(\verb!A! and \verb!B!) conflicted, and of the two of them \verb!A! is
first in the history.  The last two patches in the unwinding of any merger
are always just this easy.  So this unwinds to:
\begin{verbatim}
A
M(A,B)
\end{verbatim}
What about a merger of mergers? How about \verb!M(A,M(C,D))!.  In this case
we know the two most recent patches are:
\begin{verbatim}
A
M(A,M(C,D))
\end{verbatim}
But obviously the unwinding isn't complete, since we don't yet see where
\verb!C! and \verb!D! came from.  In this case we take the unwinding of
\verb!M(C,D)! and drop its latest patch (which is \verb!M(C,D)! itself) and
place that at the beginning of our patch train:
\begin{verbatim}
C
A
M(A,M(C,D))
\end{verbatim}
As we look at \verb!M( M(A,B), M(A,M(C,D)))!, we consider the unwindings of
each of its subpatches:
\begin{verbatim}
          C
A         A
M(A,B)    M(A,M(C,D))
\end{verbatim}
As we did with \verb!M(A,M(C,D))!, we'll drop the first patch on the
right and insert the first patch on the left.  That moves us up to the two
\verb!A!'s.  Since these agree, we can use just one of them (they
``should'' agree).  That leaves us with the \verb!C! which goes first.

The catch is that things don't always turn out this easily.  There is no
guarantee that the two \verb!A!'s would come out at the same time, and if
they didn't, we'd have to rearrange things until they did.  Or if there was
no way to rearrange things so that they would agree, we have to go on to
plan B, which I will explain now.

Consider the case of \verb!M( M(A,B), M(C,D))!.  We can easily unwind the
two subpatches
\begin{verbatim}
A         C
M(A,B)    M(C,D)
\end{verbatim}
Now we need to reconcile the \verb!A! and \verb!C!.  How do we do this?
Well, as usual, the solution is to use the most wonderful
Theorem~\ref{merge_thm}.  In this case we have to use it in the reverse of
how we used it when merging, since we know that \verb!A! and \verb!C! could
either one be the \emph{last} patch applied before \verb!M(A,B)! or
\verb!M(C,D)!.  So we can find \verb!C'! using
\[
A^{ -1} C \commutex C' A'^{ -1}
\]
Giving an unwinding of
\begin{verbatim}
C'
A
M(A,B)
M( M(A,B), M(C,D) )
\end{verbatim}
There is a bit more complexity to the unwinding process (mostly having to
do with cases where you have deeper nesting), but I think the general
principles that are followed are pretty much included in the above
discussion.

\begin{code}
unwind :: Patch prim C(x y) -> Sealed (RL (Patch prim) C(x)) -- Recreates a patch history in reverse.
unwind (Merger _ unwindings _ _) = Sealed unwindings
unwind p = Sealed (p :<: NilRL)

trueUnwind :: PrimPatch prim => Patch prim C(x y) -> Sealed (RL (Patch prim) C(x)) -- Recreates a patch history in reverse.
trueUnwind p@(Merger _ _ p1 p2) =
    case (unwind p1, unwind p2) of
    (Sealed (_:<:p1s),Sealed (_:<:p2s)) ->
         Sealed (p :<: unsafeCoerceP p1 :<: unsafeUnsealFlipped (reconcileUnwindings p p1s (unsafeCoercePEnd p2s)))
    _ -> impossible
trueUnwind _ = impossible

reconcileUnwindings :: PrimPatch prim
                    => Patch prim C(a b) -> RL (Patch prim) C(x z) -> RL (Patch prim) C(y z) -> FlippedSeal (RL (Patch prim)) C(z)
reconcileUnwindings _ NilRL p2s = FlippedSeal p2s
reconcileUnwindings _ p1s NilRL = FlippedSeal p1s
reconcileUnwindings p (p1:<:p1s) p2s@(p2:<:tp2s) =
    case [(p1s', p2s')|
          p1s'@(hp1s':<:_) <- headPermutationsRL (p1:<:p1s),
          p2s'@(hp2s':<:_) <- headPermutationsRL p2s,
          hp1s' `unsafeCompare` hp2s'] of
    ((p1':<:p1s', _:<:p2s'):_) ->
        mapFlipped (p1' :<:) $ reconcileUnwindings p p1s' (unsafeCoercePEnd p2s')
    [] -> case reverseFL `fmap` putBefore p1 (reverseRL p2s) of
          Just p2s' -> mapFlipped (p1 :<:) $ reconcileUnwindings p p1s p2s'
          Nothing ->
              case fmap reverseFL $ putBefore p2 $
                   reverseRL (p1:<:p1s) of
              Just p1s' -> mapFlipped (p2 :<:) $
                           reconcileUnwindings p p1s' tp2s
              Nothing ->
                  bugDoc $ text "in function reconcileUnwindings"
                        $$ text "Original patch:"
                        $$ showPatch_ p
    _ -> bug "in reconcileUnwindings"

-- This code seems wrong, shouldn't the commute be invert p1 :> p2 ? And why isn't p1' re-inverted?
-- it seems to have been this way forever:
-- Fri May 23 10:27:04 BST 2003  droundy@abridgegame.org
--    * fix bug in unwind and add docs on unwind algorithm.
putBefore :: PrimPatch prim => Patch prim C(y z) -> FL (Patch prim) C(x z) -> Maybe (FL (Patch prim) C(y w))
putBefore p1 (p2:>:p2s) =
    do p1' :> p2' <- commute (unsafeCoerceP p2 :> invert p1)
       _ <- commute (p2' :> p1)
       (unsafeCoerceP p2' :>:) `fmap` putBefore p1' (unsafeCoerceP p2s)
putBefore _ NilFL = Just (unsafeCoerceP NilFL)
\end{code}

\section{Conflicts}

There are a couple of simple constraints on the routine which determines
how to resolve two conflicting patches (which is called `glump').  These
must be satisfied in order that the result of a series of merges is always
independent of their order.  Firstly, the output of glump cannot change
when the order of the two conflicting patches is switched.  If it did, then
commuting the merger could change the resulting patch, which would be bad.
Secondly, the result of the merge of three (or more) conflicting patches
cannot depend on the order in which the merges are performed.

The conflict resolution code (glump) begins by ``unravelling'' the merger
into a set of sequences of patches.  Each sequence of patches corresponds
to one non-conflicted patch that got merged together with the others.  The
result of the unravelling of a series of merges must obviously be
independent of the order in which those merges are performed.  This
unravelling code (which uses the unwind code mentioned above) uses probably
the second most complicated algorithm.  Fortunately, if we can successfully
unravel the merger, almost any function of the unravelled merger satisfies
the two constraints mentioned above that the conflict resolution code must
satisfy.

\begin{code}
instance PrimPatch prim => CommuteNoConflicts (Patch prim) where
  commuteNoConflicts (x:>y) =   do x' :< y' <- commuteNoMerger (y :< x)
                                   return (y':>x')

instance PrimPatch prim => Conflict (Patch prim) where
  resolveConflicts patch = rcs NilFL (patch :<: NilRL)
    where rcs :: FL (Patch prim) C(y w) -> RL (Patch prim) C(x y) -> [[Sealed (FL prim C(w))]]
          rcs _ NilRL = []
          rcs passedby (p@(Merger _ _ _ _):<:ps) =
              case revCommuteFLId commuteNoMerger (passedby:<p) of
              Just (p'@(Merger _ _ p1 p2):<_) ->
                  (map Sealed $ nubBy unsafeCompare $
                        effect (unsafeCoercePStart $ unsafeUnseal (glump09 p1 p2)) : map (unsafeCoercePStart . unsafeUnseal) (unravel p'))
                  : rcs (p :>: passedby) ps
              Nothing -> rcs (p :>: passedby) ps
              _ -> impossible
          rcs passedby (p:<:ps) = seq passedby $
                                  rcs (p :>: passedby) ps

-- This type seems wrong - the most natural type for the result would seem to be
-- [Sealed (FL Prim C(x))], given the type of unwind.
-- However downstream code in darcs convert assumes the C(y) type, and I was unable
-- to figure out whether this could/should reasonably be changed -- Ganesh 13/4/10
publicUnravel :: PrimPatch prim => Patch prim C(x y) -> [Sealed (FL prim C(y))]
publicUnravel = map (mapSeal unsafeCoercePStart) . unravel

unravel :: PrimPatch prim => Patch prim C(x y) -> [Sealed (FL prim C(x))]
unravel p = nub $ map (mapSeal (sortCoalesceFL . concatFL . mapFL_FL effect)) $
            getSupers $ map (mapSeal reverseRL) $ unseal (newUr p) $ unwind p

getSupers :: PrimPatch prim => [Sealed (FL (Patch prim) C(x))] -> [Sealed (FL (Patch prim) C(x))]
getSupers (x:xs) =
    case filter (not.(x `isSuperpatchOf`)) xs of
    xs' -> if or $ map (`isSuperpatchOf` x) xs'
           then getSupers xs'
           else x : getSupers xs'
getSupers [] = []

isSuperpatchOf :: PrimPatch prim => Sealed (FL (Patch prim) C(x)) -> Sealed (FL (Patch prim) C(x)) -> Bool
Sealed x `isSuperpatchOf` Sealed y | lengthFL y > lengthFL x = False -- should be just an optimisation
Sealed x `isSuperpatchOf` Sealed y = x `iso` y
    where iso :: PrimPatch prim => FL (Patch prim) C(x y) -> FL (Patch prim) C(x z) -> Bool
          _ `iso` NilFL = True
          NilFL `iso` _ = False
          a `iso` (b:>:bs) =
              head $ ([as `iso` bs | (ah :>: as) <- simpleHeadPermutationsFL a, IsEq <- [ah =\/= b]] :: [Bool]) ++ [False]

merger :: PrimPatch prim => String -> Patch prim C(x y) -> Patch prim C(x z) -> Sealed (Patch prim C(y))
merger "0.0" p1 p2 = Sealed $ Merger undoit unwindings p1 p2
    where fake_p = Merger NilFL NilRL p1 p2
          unwindings = unsafeUnseal (trueUnwind fake_p)
          p = Merger NilFL unwindings p1 p2
          undoit =
              case (isMerger p1, isMerger p2) of
              (True ,True ) -> case unwind p of
                                 Sealed (_:<:t) -> unsafeCoerceP $ invertRL t
                                 _ -> impossible
              (False,False) -> unsafeCoerceP $ invert p1 :>: NilFL
              (True ,False) -> unsafeCoerceP $ NilFL
              (False,True ) -> unsafeCoerceP $ invert p1 :>: mergerUndo p2
merger g _ _ =
    error $ "Cannot handle mergers other than version 0.0\n"++g
    ++ "\nPlease use darcs optimize --modernize with an older darcs."

glump09 :: PrimPatch prim => Patch prim C(x y) -> Patch prim C(x z) -> Sealed (FL (Patch prim) C(y))
glump09 p1 p2 = mapSeal (mapFL_FL fromPrim) $ mangleUnravelled $ unseal unravel $ merger "0.0" p1 p2

instance PrimPatch prim => Effect (Patch prim) where
    effect p@(Merger _ _ _ _) = sortCoalesceFL $ effect $ mergerUndo p
    effect p@(Regrem _ _ _ _) = invert $ effect $ invert p
    effect (PP p) = p :>: NilFL

instance IsHunk prim => IsHunk (Patch prim) where
    isHunk p = do PP p' <- return p
                  isHunk p'

newUr :: PrimPatch prim => Patch prim C(a b) -> RL (Patch prim) C(x y) -> [Sealed (RL (Patch prim) C(x))]
newUr p (Merger _ _ p1 p2 :<: ps) =
   case filter (\(pp:<:_) -> pp `unsafeCompare` p1) $ headPermutationsRL ps of
   ((_:<:ps'):_) -> newUr p (unsafeCoercePStart p1:<:ps') ++ newUr p (unsafeCoercePStart p2:<:ps')
   _ -> bugDoc $ text "in function newUr"
              $$ text "Original patch:"
              $$ showPatch_ p
              $$ text "Unwound:"
              $$ vcat (unseal (mapRL showPatch_) $ unwind p)

newUr op ps =
    case filter (\(p:<:_) -> isMerger p) $ headPermutationsRL ps of
    [] -> [Sealed ps]
    (ps':_) -> newUr op ps'

instance Invert prim => Invert (Patch prim) where
    invert (Merger undo unwindings p1 p2)
        = Regrem undo unwindings p1 p2
    invert (Regrem undo unwindings p1 p2)
        = Merger undo unwindings p1 p2
    invert (PP p) = PP (invert p)

instance MyEq prim => MyEq (Patch prim) where
    unsafeCompare = eqPatches

instance MyEq prim => Eq (Patch prim C(x y)) where
    (==) = unsafeCompare

eqPatches :: MyEq prim => Patch prim C(x y) -> Patch prim C(w z) -> Bool
eqPatches (PP p1) (PP p2) = unsafeCompare p1 p2
eqPatches (Merger _ _ p1a p1b) (Merger _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches (Regrem _ _ p1a p1b) (Regrem _ _ p2a p2b)
 = eqPatches p1a p2a &&
   eqPatches p1b p2b
eqPatches _ _ = False

\end{code}
