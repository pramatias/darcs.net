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

module Darcs.Patch.Show
     ( ShowPatchBasic(..), ShowPatch(..)
     , showNamedPrefix
     , writePatch, gzWritePatch
     , formatFileName
     )
    where

import Prelude hiding ( pi )

import Darcs.Lock ( writeDocBinFile, gzWriteDocFile )
import Darcs.Patch.FileName ( FileName, fn2ps, encodeWhite, fn2fp )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo )
import Darcs.Witnesses.Ordered ( FL )

import English ( plural, Noun(Noun) )
import Printer ( Doc, vcat, blueText, ($$), (<>), text, packedString )
import Storage.Hashed.Monad( TreeIO )


#include "gadts.h"

\end{code}

\paragraph{Named patches}

Named patches are displayed as a ``patch id'' which is in square brackets,
followed by a patch.  Optionally, after the patch id (but before the patch
itself) can come a list of dependencies surrounded by angle brackets.  Each
dependency consists of a patch id.

\begin{code}
showNamedPrefix :: PatchInfo -> [PatchInfo] -> Doc
showNamedPrefix n d = showPatchInfo n
                   $$ blueText "<"
                   $$ vcat (map showPatchInfo d)
                   $$ blueText ">"

class ShowPatchBasic p where
    showPatch :: p C(x y) -> Doc

class ShowPatchBasic p => ShowPatch p where
    showNicely :: p C(x y) -> Doc
    showNicely = showPatch
    -- | showContextPatch is used to add context to a patch, as diff
    -- -u does. Thus, it differs from showPatch only for hunks. It is
    -- used for instance before putting it into a bundle. As this
    -- unified context is not included in patch representation, this
    -- requires access to the tree.
    showContextPatch :: p C(x y) -> TreeIO Doc
    showContextPatch p = return $ showPatch p
    description :: p C(x y) -> Doc
    description = showPatch
    summary :: p C(x y) -> Doc
    summaryFL :: FL p C(x y) -> Doc
    thing :: p C(x y) -> String
    thing _ = "patch"
    things :: p C(x y) -> String
    things x = plural (Noun $ thing x) ""

writePatch :: ShowPatchBasic p => FilePath -> p C(x y) -> IO ()
writePatch f p = writeDocBinFile f $ showPatch p <> text "\n"
gzWritePatch :: ShowPatchBasic p => FilePath -> p C(x y) -> IO ()
gzWritePatch f p = gzWriteDocFile f $ showPatch p <> text "\n"

formatFileName :: FileNameFormat -> FileName -> Doc
formatFileName OldFormat = packedString . fn2ps
formatFileName NewFormat = text . encodeWhite . fn2fp

\end{code}