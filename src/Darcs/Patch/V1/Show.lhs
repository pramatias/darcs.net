\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.V1.Show ( showPatch_ ) where

import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Prim ( showPrim, PrimPatch )

import Darcs.Patch.V1.Core ( Patch(..) )

import Darcs.Witnesses.Show ( Show1(..), Show2(..), ShowDict(..) )

import Printer ( Doc, renderString,
                 text, blueText,
                 ($$), (<+>) )

#include "gadts.h"
\end{code}

\section{Patch string formatting}

Of course, in order to store our patches in a file, we'll have to save them
as some sort of strings.  The convention is that each patch string will end
with a newline, but on parsing we skip any amount of whitespace between
patches.
\begin{code}
instance PrimPatch prim => Show (Patch prim C(x y))  where
    show p = renderString (showPatch_ p) ++ "\n"
instance PrimPatch prim => Show1 (Patch prim C(x)) where
    showDict1 = ShowDictClass
instance PrimPatch prim => Show2 (Patch prim) where
    showDict2 = ShowDictClass

showPatch_ :: PrimPatch prim => Patch prim C(a b) -> Doc
showPatch_ (PP p) = showPrim OldFormat p
showPatch_ (Merger _ _ p1 p2) = showMerger "merger" p1 p2
showPatch_ (Regrem _ _ p1 p2) = showMerger "regrem" p1 p2
\end{code}

\paragraph{Merger patches}
Merge two patches.  The MERGERVERSION is included to allow some degree of
backwards compatibility if the merger algorithm needs to be changed.
\begin{verbatim}
merger MERGERVERSION
<first patch>
<second patch>
\end{verbatim}
\begin{code}
showMerger :: PrimPatch prim => String -> Patch prim C(a b) -> Patch prim C(d e) -> Doc
showMerger merger_name p1 p2 =
    blueText merger_name <+> text "0.0" <+> blueText "("
                           $$ showPatch_ p1
                           $$ showPatch_ p2
                           $$ blueText ")"
\end{code}
