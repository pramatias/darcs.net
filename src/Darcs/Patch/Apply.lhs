%  Copyright (C) 2002-2005 David Roundy
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
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Patch.Apply ( Apply(..),
                           applyToFilepaths,
                           applyToTree,
                         )
    where

import Prelude hiding ( catch, pi )

import Darcs.FilePathMonad ( withFilePaths )
import Darcs.Witnesses.Ordered ( FL(..), RL(..) )

import Darcs.IO ( WriteableDirectory(..) )
import Storage.Hashed.Tree( Tree )
import Storage.Hashed.Monad( virtualTreeIO )

\end{code}



\section{Introduction}

A patch describes a change to the tree.  It could be either a primitive
patch (such as a file add/remove, a directory rename, or a hunk replacement
within a file), or a composite patch describing many such changes.  Every
patch type must satisfy the conditions described in this appendix.  The
theory of patches is independent of the data which the patches manipulate,
which is what makes it both powerful and useful, as it provides a framework
upon which one can build a revision control system in a sane manner.

Although in a sense, the defining property of any patch is that it can be
applied to a certain tree, and thus make a certain change, this change does
not wholly define the patch.  A patch is defined by a
\emph{representation}, together with a set of rules for how it behaves
(which it has in common with its patch type).  The \emph{representation} of
a patch defines what change that particular patch makes, and must be
defined in the context of a specific tree.  The theory of patches is a
theory of the many ways one can change the representation of a patch to
place it in the context of a different tree.  The patch itself is not
changed, since it describes a single change, which must be the same
regardless of its representation\footnote{For those comfortable with
quantum mechanics, think of a patch as a quantum mechanical operator, and
the representation as the basis set.  The analogy breaks down pretty
quickly, however, since an operator could be described in any complete
basis set, while a patch modifying the file {\tt foo} can only be described
in the rather small set of contexts which have a file {\tt foo} to be
modified.}.

So how does one define a tree, or the context of a patch? The simplest way
to define a tree is as the result of a series of patches applied to the
empty tree\footnote{This is very similar to the second-quantized picture,
in which any state is seen as the result of a number of creation operators
acting on the vacuum, and provides a similar set of simplifications---in
particular, the exclusion principle is very elegantly enforced by the
properties of the anti-hermitian fermion creation operators.}.  Thus, the
context of a patch consists of the set of patches that precede it.

\section{Applying patches}


\begin{code}

class Apply p where
    apply :: WriteableDirectory m => p C(x y) -> m ()

instance Apply p => Apply (FL p) where
    apply NilFL = return ()
    apply (p:>:ps) = apply p >> apply ps

instance Apply p => Apply (RL p) where
    apply NilRL = return ()
    apply (p:<:ps) = apply ps >> apply p

applyToFilepaths :: Apply p => p C(x y) -> [FilePath] -> [FilePath]
applyToFilepaths pa fs = withFilePaths fs (apply pa)

\end{code}

\input{Darcs/Patch/Prim/V1/Apply.lhs}
\input{Darcs/Patch/Prim/V1/Details.lhs}

\begin{code}

-- | Apply a patch to a 'Tree', yielding a new 'Tree'.
applyToTree :: (Apply p) => p C(x y) -> Tree IO -> IO (Tree IO)
applyToTree patch t = snd `fmap` virtualTreeIO (apply patch) t

\end{code}

