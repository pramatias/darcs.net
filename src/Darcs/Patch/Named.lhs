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


\section{Patch relationships}

\begin{code}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.Named
       ( Named(..),
         infopatch,
         adddeps, namepatch, anonymous,
         getdeps,
         patch2patchinfo, patchname, patchcontents,
         fmapNamed, fmapFL_Named
       )
       where

import Prelude hiding ( pi )
import Darcs.Patch.Conflict ( Conflict(..), CommuteNoConflicts )
import Darcs.Patch.Effect ( Effect(effect, effectRL) )
import Darcs.Patch.FileHunk ( IsHunk(..) )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Info ( PatchInfo, readPatchInfo, showPatchInfo, patchinfo,
                          humanFriendly, makePatchname, invertName )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Patchy, Commute(..), Invert(..), Apply(..),
                            PatchInspect(..), ReadPatch(..) )
import Darcs.Patch.Prim ( PrimOf, PrimPatchBase )
import Darcs.Patch.ReadMonads ( ParserM, option, lexChar,
                                choice, skipWhile, anyChar )
import Darcs.Patch.Repair ( mapMaybeSnd, Repair(..), RepairToFL, Check(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), showNamedPrefix )
import Darcs.Patch.Summary ( plainSummary )
import Darcs.Patch.Viewing () -- for ShowPatch FL instances

import Darcs.Witnesses.Eq ( MyEq(..) )
import Darcs.Witnesses.Ordered ( (:>)(..), (:\/:)(..), (:/\:)(..), FL, mapFL, mapFL_FL )
import Darcs.Witnesses.Sealed ( Sealed, mapSeal )
import Darcs.Witnesses.Show ( ShowDict(..), Show1(..), Show2(..) )

import Printer ( renderString, ($$), (<+>), (<>), prefix, text, vcat )

-- | The @Named@ type adds a patch info about a patch, that is a name.
data Named p C(x y) where
    NamedP :: !PatchInfo
           -> ![PatchInfo]
           -> !(FL p C(x y))
           -> Named p C(x y)
-- ^ @NamedP info deps p@ represents patch @p@ with name
-- @info@. @deps@ is a list of dependencies added at the named patch
-- level, compared with the unnamed level (ie, dependencies added with
-- @darcs record --ask-deps@).

instance PrimPatchBase p => PrimPatchBase (Named p) where
    type PrimOf (Named p) = PrimOf p

instance Effect p => Effect (Named p) where
    effect (NamedP _ _ p) = effect p
    effectRL (NamedP _ _ p) = effectRL p

instance IsHunk (Named p) where
    isHunk _ = Nothing

instance PatchListFormat (Named p)

instance (ReadPatch p, PatchListFormat p) => ReadPatch (Named p) where
 readPatch' = readNamed

readNamed :: (ReadPatch p, PatchListFormat p, ParserM m) => m (Sealed (Named p C(x )))
readNamed
          = do n <- readPatchInfo
               d <- readDepends
               p <- readPatch'
               return $ (NamedP n d) `mapSeal` p

readDepends :: ParserM m => m [PatchInfo]
readDepends =
  option [] $ do lexChar '<'
                 readPis

readPis :: ParserM m => m [PatchInfo]
readPis = choice [ do pi <- readPatchInfo
                      pis <- readPis
                      return (pi:pis)
                 , do skipWhile (/= '>')
                      _ <- anyChar
                      return [] ]

instance Apply p => Apply (Named p) where
    apply (NamedP _ _ p) = apply p

instance RepairToFL p => Repair (Named p) where
    applyAndTryToFix (NamedP n d p) = mapMaybeSnd (NamedP n d) `fmap` applyAndTryToFix p

infopatch :: Patchy p => PatchInfo -> FL p C(x y) -> Named p C(x y)
adddeps :: Named p C(x y) -> [PatchInfo] -> Named p C(x y)
getdeps :: Named p C(x y) -> [PatchInfo]

namepatch :: Patchy p => String -> String -> String -> [String] -> FL p C(x y) -> IO (Named p C(x y))
namepatch date name author desc p
    | '\n' `elem` name = error "Patch names cannot contain newlines."
    | otherwise = do pinf <- patchinfo date name author desc
                     return $ NamedP pinf [] p

anonymous :: Patchy p => FL p C(x y) -> IO (Named p C(x y))
anonymous p = namepatch "today" "anonymous" "unknown" ["anonymous"] p

infopatch pi p = NamedP pi [] p
adddeps (NamedP pi _ p) ds = NamedP pi ds p
getdeps (NamedP _ ds _) = ds

patch2patchinfo :: Named p C(x y) -> PatchInfo
patch2patchinfo (NamedP i _ _) = i

patchname :: Named p C(x y) -> String
patchname (NamedP i _ _) = makePatchname i

patchcontents :: Named p C(x y) -> FL p C(x y)
patchcontents (NamedP _ _ p) = p

fmapNamed :: (FORALL(a b) p C(a b) -> q C(a b)) -> Named p C(x y) -> Named q C(x y)
fmapNamed f (NamedP i deps p) = NamedP i deps (mapFL_FL f p)

fmapFL_Named :: (FL p C(x y) -> FL q C(x y)) -> Named p C(x y) -> Named q C(x y)
fmapFL_Named f (NamedP i deps p) = NamedP i deps (f p)

\end{code}

The simplest relationship between two patches is that of ``sequential''
patches, which means that the context of the second patch (the one on the
left) consists of the first patch (on the right) plus the context of the
first patch.  The composition of two patches (which is also a patch) refers
to the patch which is formed by first applying one and then the other.  The
composition of two patches, $P_1$ and $P_2$ is represented as $P_2P_1$,
where $P_1$ is to be applied first, then $P_2$\footnote{This notation is
inspired by the notation of matrix multiplication or the application of
operators upon a Hilbert space.  In the algebra of patches, there is
multiplication (i.e.\ composition), which is associative but not
commutative, but no addition or subtraction.}

There is one other very useful relationship that two patches can have,
which is to be parallel patches, which means that the two patches have an
identical context (i.e.\ their representation applies to identical trees).
This is represented by $P_1\parallel P_2$.  Of course, two patches may also
have no simple relationship to one another.  In that case, if you want to
do something with them, you'll have to manipulate them with respect to
other patches until they are either in sequence or in parallel.

The most fundamental and simple property of patches is that they must be
invertible.  The inverse of a patch is described by: $P^{ -1}$.  In the
darcs implementation, the inverse is required to be computable from
knowledge of the patch only, without knowledge of its context, but that
(although convenient) is not required by the theory of patches.
\begin{dfn}
The inverse of patch $P$ is $P^{ -1}$, which is the ``simplest'' patch for
which the composition \( P^{ -1} P \) makes no changes to the tree.
\end{dfn}
Using this definition, it is trivial to prove the following theorem
relating to the inverse of a composition of two patches.
\begin{thm} The inverse of the composition of two patches is
\[ (P_2 P_1)^{ -1} = P_1^{ -1} P_2^{ -1}. \]
\end{thm}
Moreover, it is possible to show that the right inverse of a patch is equal
to its left inverse.  In this respect, patches continue to be analogous to
square matrices, and indeed the proofs relating to these properties of the
inverse are entirely analogous to the proofs in the case of matrix
multiplication.  The compositions proofs can also readily be extended to
the composition of more than two patches.
\begin{code}

instance (Commute p, MyEq p) => MyEq (Named p) where
    unsafeCompare (NamedP n1 d1 p1) (NamedP n2 d2 p2) =
        n1 == n2 && d1 == d2 && unsafeCompare p1 p2

instance (Commute p, Invert p) => Invert (Named p) where
    invert (NamedP n d p)  = NamedP (invertName n) (map invertName d) (invert p)


instance Commute p => Commute (Named p) where
    commute (NamedP n1 d1 p1 :> NamedP n2 d2 p2) =
        if n2 `elem` d1 || n1 `elem` d2
        then Nothing
        else do (p2' :> p1') <- commute (p1 :> p2)
                return (NamedP n2 d2 p2' :> NamedP n1 d1 p1')

instance Merge p => Merge (Named p) where
    merge (NamedP n1 d1 p1 :\/: NamedP n2 d2 p2)
        = case merge (p1 :\/: p2) of
          (p2' :/\: p1') -> NamedP n2 d2 p2' :/\: NamedP n1 d1 p1'

instance PatchInspect p => PatchInspect (Named p) where
    listTouchedFiles (NamedP _ _ p) = listTouchedFiles p
    hunkMatches f (NamedP _ _ p) = hunkMatches f p

instance (CommuteNoConflicts p, Conflict p) => Conflict (Named p) where
    listConflictedFiles (NamedP _ _ p) = listConflictedFiles p
    resolveConflicts (NamedP _ _ p) = resolveConflicts p

instance Check p => Check (Named p) where
    isInconsistent (NamedP _ _ p) = isInconsistent p

instance (PatchListFormat p, ShowPatchBasic p) => ShowPatchBasic (Named p) where
    showPatch (NamedP n [] p) = showPatchInfo n <> showPatch p
    showPatch (NamedP n d p) = showNamedPrefix n d <+> showPatch p

instance (Apply p, CommuteNoConflicts p, Conflict p, IsHunk p, PatchListFormat p, PrimPatchBase p, ShowPatch p) => ShowPatch (Named p) where
    showContextPatch (NamedP n [] p) = showContextPatch p >>= return . (showPatchInfo n <>)
    showContextPatch (NamedP n d p) = showContextPatch p >>= return . (showNamedPrefix n d <+>)
    description (NamedP n _ _) = humanFriendly n
    summary p = description p $$ text "" $$
                prefix "    " (plainSummary p) -- this isn't summary because summary does the
                                            -- wrong thing with (Named (FL p)) so that it can
                                            -- get the summary of a sequence of named patches
                                            -- right.
    summaryFL = vcat . mapFL summary
    showNicely p@(NamedP _ _ pt) = description p $$
                                   prefix "    " (showNicely pt)

instance (PatchListFormat p, ShowPatch p) => Show (Named p C(x y)) where
    show = renderString . showPatch

instance (PatchListFormat p, ShowPatch p) => Show1 (Named p C(x)) where
    showDict1 = ShowDictClass

instance (PatchListFormat p, ShowPatch p) => Show2 (Named p) where
    showDict2 = ShowDictClass

\end{code}
