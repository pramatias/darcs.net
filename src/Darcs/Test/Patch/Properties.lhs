%  Copyright (C) 2007 David Roundy
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

\documentclass{article}
%\usepackage{color}

\usepackage{verbatim}
\usepackage{html}
\usepackage{fancyvrb}
\newenvironment{code}{\comment}{\endcomment}
% \newenvironment{code}{\color{blue}\verbatim}{\endverbatim}

\newcommand{\commutes}{\longleftrightarrow}

\begin{document}

\begin{code}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}

#include "gadts.h"

module Darcs.Test.Patch.Properties
    ( recommute, commuteInverses, permutivity, partialPermutivity,
      patchAndInverseCommute, mergeEitherWay,
      show_read,
      mergeCommute, mergeConsistent, mergeArgumentsConsistent,
      joinCommute
    ) where

import Control.Monad ( msum, mplus )
import Darcs.Witnesses.Show ( Show2(..), show2 )
import Darcs.Patch.Patchy ( Patchy, showPatch, commute, invert )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch ()
import Darcs.Patch.Commute ( commuteFLorComplain )
import Darcs.Patch.Merge ( Merge(merge) )
import Darcs.Patch.Read ( readPatch )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Ordered ( FL(..), (:>)(..), (:\/:)(..), (:/\:)(..) )
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
import Printer ( Doc, renderPS, redText, greenText, ($$) )
--import Darcs.ColorPrinter ( traceDoc )
\end{code}

\section{Patch properties}

Darcs is built on a hierarchy of patch types.  At the lowest level are
``primitive'' patches, and from these building blocks, a whole hierarchy of
patch types are built.  Each of these patch types must support a number of
functions, which must obey a number of laws.

\subsection{Commute properties}

\begin{prp}[Recommute]
  $AB \commutes B'A'$ if and only if $B'A' \commutes AB$
\end{prp}

\begin{code}
recommute :: Patchy p => (FORALL(x y) ((p :> p) C(x y) -> Maybe ((p :> p) C(x y))))
          -> (p :> p) C(a b) -> Maybe Doc
recommute c (x :> y) =
    case c (x :> y) of
    Nothing -> Nothing
    Just (y' :> x') ->
       case c (y' :> x') of
         Nothing -> Just (redText "failed" $$ showPatch y' $$ redText ":>" $$ showPatch x')
         Just (x'' :> y'') ->
             case y'' =/\= y of
             NotEq -> Just (redText "y'' =/\\= y failed, where x" $$ showPatch x $$
                            redText ":> y" $$ showPatch y $$
                            redText "y'" $$ showPatch y' $$
                            redText ":> x'" $$ showPatch x' $$
                            redText "x''" $$ showPatch x'' $$
                            redText ":> y''" $$ showPatch y'')
             IsEq -> case x'' =/\= x of
                     NotEq -> Just (redText "x'' /= x" $$ showPatch x'' $$ redText ":>" $$ showPatch y'')
                     IsEq -> Nothing
\end{code}

\begin{prp}[Commute inverses]
  $AB \commutes B'A'$ if and only if $B^{-1}A^{-1} \commutes A'^{-1}B'^{-1}$
\end{prp}

\begin{code}
commuteInverses :: Patchy p => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                 -> (p :> p) C(a b) -> Maybe Doc
commuteInverses c (x :> y) =
    case c (x :> y) of
    Nothing -> Nothing
    Just (y' :> x') ->
        case c (invert y :> invert x) of
        Nothing -> Just $ redText "second commute failed" $$
                   redText "x" $$ showPatch x $$ redText "y" $$ showPatch y $$
                   redText "y'" $$ showPatch y' $$ redText "x'" $$ showPatch x'
        Just (ix' :> iy') ->
            case invert ix' =/\= x' of
            NotEq -> Just $ redText "invert ix' /= x'" $$
                     redText "x" $$ showPatch x $$
                     redText "y" $$ showPatch y $$
                     redText "y'" $$ showPatch y' $$
                     redText "x'" $$ showPatch x' $$
                     redText "ix'" $$ showPatch ix' $$
                     redText "iy'" $$ showPatch iy' $$
                     redText "invert ix'" $$ showPatch (invert ix') $$
                     redText "invert iy'" $$ showPatch (invert iy')
            IsEq -> case y' =\/= invert iy' of
                    NotEq -> Just $ redText "y' /= invert iy'" $$ showPatch iy' $$ showPatch y'
                    IsEq -> Nothing
\end{code}

\begin{prp}[Patch and inverse]
  If $AB \commutes B'A'$ then $A^{-1}B' \commutes BA'^{-1}$
\end{prp}

This property is only true of primitive patches.

\begin{code}
patchAndInverseCommute :: Patchy p =>
                             (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                          -> (p :> p) C(a b) -> Maybe Doc
patchAndInverseCommute c (x :> y) =
  do y' :> x' <- c (x :> y)
     case c (invert x :> y') of
       Nothing -> Just (redText "failure in patchAndInverseCommute")
       Just (y'' :> ix') ->
           case y'' =\/= y of
           NotEq -> Just (redText "y'' /= y" $$
                          redText "x" $$ showPatch x $$
                          redText "y" $$ showPatch y $$
                          redText "x'" $$ showPatch x' $$
                          redText "y'" $$ showPatch y' $$
                          redText "y''" $$ showPatch y'' $$
                          redText ":> x'" $$ showPatch x')
           IsEq -> case x' =\/= invert ix' of
                   NotEq -> Just (redText "x' /= invert ix'" $$
                                  redText "y''" $$ showPatch y'' $$
                                  redText ":> x'" $$ showPatch x' $$
                                  redText "invert x" $$ showPatch (invert x) $$
                                  redText ":> y" $$ showPatch y $$
                                  redText "y'" $$ showPatch y' $$
                                  redText "ix'" $$ showPatch ix')
                   IsEq -> Nothing
\end{code}

\begin{prp}[Permutivity]
  (to be added...)
\end{prp}

\begin{code}
permutivity :: Patchy p => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
            -> (p :> p :> p) C(a b) -> Maybe Doc
permutivity c (x:>y:>z) =
  do y1 :> x1 <- c (x :> y)
     z2 :> y2 <- --traceDoc (greenText "first commuted") $
                 c (y :> z)
     z3 :> x3 <- --traceDoc (greenText "second commuted") $
                 c (x :> z2)
     case c (x1 :> z) of
       Nothing -> Just $ redText "permutivity1"
       Just (z4 :> x4) ->
         --traceDoc (greenText "third commuted" $$
         --          greenText "about to commute" $$
         --          greenText "y1" $$ showPatch y1 $$
         --          greenText "z4" $$ showPatch z4) $
         case c (y1 :> z4) of
         Nothing -> Just $ redText "permutivity2"
         Just (z3_ :> y4)
             | IsEq <- z3_ =\/= z3 ->
                  --traceDoc (greenText "passed z3") $ error "foobar test" $
                  case c (y4 :> x4) of
                  Nothing -> Just $ redText "permutivity5: input was" $$
                             redText "x" $$ showPatch x $$
                             redText "y" $$ showPatch y $$
                             redText "z" $$ showPatch z $$
                             redText "z3" $$ showPatch z3 $$
                             redText "failed commute of" $$
                             redText "y4" $$ showPatch y4 $$
                             redText "x4" $$ showPatch x4 $$
                             redText "whereas commute of x and y give" $$
                             redText "y1" $$ showPatch y1 $$
                             redText "x1" $$ showPatch x1
                  Just (x3_ :> y2_)
                       | NotEq <- x3_ =\/= x3 -> Just $ redText "permutivity6"
                       | NotEq <- y2_ =/\= y2 -> Just $ redText "permutivity7"
                       | otherwise -> Nothing
             | otherwise ->
                 Just $ redText "permutivity failed" $$
                        redText "z3" $$ showPatch z3 $$
                        redText "z3_" $$ showPatch z3_

partialPermutivity :: Patchy p => (FORALL(x y) (p :> p) C(x y) -> Maybe ((p :> p) C(x y)))
                    -> (p :> p :> p) C(a b) -> Maybe Doc
partialPermutivity c (xx:>yy:>zz) = pp (xx:>yy:>zz) `mplus` pp (invert zz:>invert yy:>invert xx)
    where pp (x:>y:>z) = do z1 :> y1 <- c (y :> z)
                            _ :> x1 <- c (x :> z1)
                            case c (x :> y) of
                              Just _ -> Nothing -- this is covered by full permutivity test above
                              Nothing ->
                                  case c (x1 :> y1) of
                                  Nothing -> Nothing
                                  Just _ -> Just $ greenText "partialPermutivity error" $$
                                            greenText "x" $$ showPatch x $$
                                            greenText "y" $$ showPatch y $$
                                            greenText "z" $$ showPatch z

mergeArgumentsConsistent :: Patchy p =>
                              (FORALL(x y) p C(x y) -> Maybe Doc)
                           -> (p :\/: p) C(a b) -> Maybe Doc
mergeArgumentsConsistent isConsistent (x :\/: y) =
    msum [(\z -> redText "mergeArgumentsConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
          (\z -> redText "mergeArgumentsConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y]

mergeConsistent :: (Patchy p, Merge p) =>
                           (FORALL(x y) p C(x y) -> Maybe Doc)
                        -> (p :\/: p) C(a b) -> Maybe Doc
mergeConsistent isConsistent (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' ->
        msum [(\z -> redText "mergeConsistent x" $$ showPatch x $$ z) `fmap` isConsistent x,
              (\z -> redText "mergeConsistent y" $$ showPatch y $$ z) `fmap` isConsistent y,
              (\z -> redText "mergeConsistent x'" $$ showPatch x' $$ z $$
                     redText "where x' comes from x" $$ showPatch x $$
                     redText "and y" $$ showPatch y) `fmap` isConsistent x',
              (\z -> redText "mergeConsistent y'" $$ showPatch y' $$ z) `fmap` isConsistent y']

mergeEitherWay :: (Patchy p, Merge p) => (p :\/: p) C(x y) -> Maybe Doc
mergeEitherWay (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' -> case merge (y :\/: x) of
                  x'' :/\: y'' | IsEq <- x'' =\/= x',
                                 IsEq <- y'' =\/= y' -> Nothing
                               | otherwise -> Just $ redText "mergeEitherWay bug"

mergeCommute :: (Patchy p, Merge p) => (p :\/: p) C(x y) -> Maybe Doc
mergeCommute (x :\/: y) =
    case merge (x :\/: y) of
    y' :/\: x' ->
        case commute (x :> y') of
        Nothing -> Just $ redText "mergeCommute 1" $$
                   redText "x" $$ showPatch x $$
                   redText "y" $$ showPatch y $$
                   redText "x'" $$ showPatch x' $$
                   redText "y'" $$ showPatch y'
        Just (y_ :> x'_)
            | IsEq <- y_ =\/= y,
              IsEq <- x'_ =\/= x' ->
                      case commute (y :> x') of
                      Nothing -> Just $ redText "mergeCommute 2 failed" $$
                                 redText "x" $$ showPatch x $$
                                 redText "y" $$ showPatch y $$
                                 redText "x'" $$ showPatch x' $$
                                 redText "y'" $$ showPatch y'
                      Just (x_ :> y'_)
                           | IsEq <- x_ =\/= x,
                             IsEq <- y'_ =\/= y' -> Nothing
                           | otherwise -> Just $ redText "mergeCommute 3" $$
                                          redText "x" $$ showPatch x $$
                                          redText "y" $$ showPatch y $$
                                          redText "x'" $$ showPatch x' $$
                                          redText "y'" $$ showPatch y' $$
                                          redText "x_" $$ showPatch x_ $$
                                          redText "y'_" $$ showPatch y'_
            | otherwise -> Just $ redText "mergeCommute 4" $$
                           redText "x" $$ showPatch x $$
                           redText "y" $$ showPatch y $$
                           redText "x'" $$ showPatch x' $$
                           redText "y'" $$ showPatch y' $$
                           redText "x'_" $$ showPatch x'_ $$
                           redText "y_" $$ showPatch y_

joinCommute :: (FORALL(x y) (Prim :> Prim) C(x y) -> Maybe (FL Prim C(x y)))
             -> (Prim :> Prim :> Prim) C(a b) -> Maybe Doc
joinCommute j (a :> b :> c) =
    do x <- j (b :> c)
       case commuteFLorComplain (a :> b :>: c :>: NilFL) of
        Right (b' :>: c' :>: NilFL :> a') ->
           case commute (a:>:NilFL :> x) of
             Just (x' :> a'':>:NilFL) ->
                 case a'' =/\= a' of
                 NotEq -> Just $ greenText "joinCommute 3"
                 IsEq -> case j (b' :> c') of
                         Nothing -> Just $ greenText "joinCommute 4"
                         Just x'' -> case x' =\/= x'' of
                                     NotEq -> Just $ greenText "joinCommute 5"
                                     IsEq -> Nothing
             _ -> Just $ greenText "joinCommute 1"
        _ -> Nothing

show_read :: (Show2 p, Patchy p) => p C(a b) -> Maybe Doc
show_read p = let ps = renderPS (showPatch p)
              in case readPatch ps of
                 Nothing -> Just (redText "unable to read " $$ showPatch p)
                 Just (Sealed p'  ) | IsEq <- p' =\/= p -> Nothing
                                    | otherwise -> Just $ redText "trouble reading patch p" $$
                                                          showPatch p $$
                                                          redText "reads as p'" $$
                                                          showPatch p' $$
                                                          redText "aka" $$
                                                          greenText (show2 p) $$
                                                          redText "and" $$
                                                          greenText (show2 p')
\end{code}

\end{document}
