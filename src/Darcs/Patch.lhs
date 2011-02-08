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

\chapter{Theory of patches}
\label{Patch}

\newtheorem{thm}{Theorem}
\newtheorem{dfn}{Definition}

\section{Background}

I think a little background on the author is in order.  I am a physicist,
and think like a physicist.  The proofs and theorems given here are what I
would call ``physicist'' proofs and theorems, which is to say that while
the proofs may not be rigorous, they are practical, and the theorems are
intended to give physical insight.  It would be great to have a
mathematician work on this to give patch theory better formalized
foundations.

From the beginning of this theory, which originated as the result of a
series of email discussions with Tom Lord, I have looked at patches as
being analogous to the operators of quantum mechanics.  I include in this
appendix footnotes explaining the theory of patches in terms of the theory
of quantum mechanics.  I advise against taking this analogy too seriously,
although it could provide some insight into how a physicist might think
about darcs.

\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
#include "gadts.h"
module Darcs.Patch ( RepoPatch,
                     PrimOf, Named, Patchy,
                     joinPatches,
                     fromPrim, fromPrims,
               rmfile, addfile, rmdir, adddir, move,
               hunk, tokreplace, namepatch, anonymous,
               binary,
               description,
               showContextPatch, showPatch, showNicely,
               infopatch, changepref,
               thing, things,
               primIsAddfile, primIsHunk, primIsSetpref,
               merge,
               commute, listTouchedFiles, hunkMatches,
               forceTokReplace,
               PrimPatch,
               -- for PatchTest
               resolveConflicts,
               Effect, effect,
               primIsBinary, gzWritePatch, writePatch, primIsAdddir,
               invert, invertFL, invertRL,
               commuteFLorComplain, commuteRL,
               readPatch, readPatchPartial,
               canonize, sortCoalesceFL,
               tryToShrink,
               patchname, patchcontents,
               applyToFilepaths, apply,
               applyToTree,
               patch2patchinfo,
               LineMark(AddedLine, RemovedLine, AddedRemovedLine, None),
               MarkedUpFile, markupFile, emptyMarkedupFile,
               summary, summaryFL, plainSummary, xmlSummary, plainSummaryPrims,
               adddeps, getdeps,
               listConflictedFiles,
               isInconsistent,
             ) where
import Darcs.Patch.Commute ( commuteFLorComplain, commuteRL )
import Darcs.Patch.Invert ( invertRL, invertFL )
import Darcs.Patch.Named ( Named,
                           adddeps, namepatch,
                           anonymous,
                           getdeps,
                           infopatch,
                           patch2patchinfo, patchname, patchcontents )
import Darcs.Patch.Read ( readPatch, readPatchPartial )
import Darcs.Patch.Patchy ( Patchy,
                            showPatch, showNicely, showContextPatch,
                            invert,
                            thing, things,
                            apply,
                            description, summary, summaryFL,
                            commute, listTouchedFiles, hunkMatches
                          )
import Darcs.Patch.RepoPatch ( RepoPatch )
import Darcs.Patch.Show ( writePatch, gzWritePatch )
import Darcs.Patch.Summary ( xmlSummary, plainSummary, plainSummaryPrims )
import Darcs.Patch.Apply ( applyToFilepaths, applyToTree )
import Darcs.Patch.MarkupData ( LineMark(..), MarkedUpFile, emptyMarkedupFile )
import Darcs.Patch.Markup ( markupFile )
import Darcs.Patch.V1.Commute ( merge )
import Darcs.Patch.Conflict ( Conflict, CommuteNoConflicts, listConflictedFiles, resolveConflicts )
import Darcs.Patch.Effect ( Effect(effect) )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Prim ( FromPrims, fromPrims, joinPatches, FromPrim, fromPrim,
                          canonize,
                          sortCoalesceFL,
                          rmdir, rmfile, tokreplace, adddir, addfile,
                          binary, changepref, hunk, move,
                          primIsAdddir, primIsAddfile,
                          primIsHunk, primIsBinary, primIsSetpref,
                          tryToShrink,
                          PrimPatch, PrimPatchBase(..) )
import Darcs.Patch.TokenReplace ( forceTokReplace )
import Darcs.Patch.Repair ( isInconsistent )

instance (CommuteNoConflicts p, Conflict p, IsHunk p, PatchListFormat p, PrimPatchBase p, Patchy p) => Patchy (Named p)
\end{code}

\input{Darcs/Patch/Apply.lhs}
\input{Darcs/Patch/Named.lhs}
\input{Darcs/Patch/V1/Core.lhs}
\input{Darcs/Patch/Prim/V1/Core.lhs}
\input{Darcs/Patch/V1/Commute.lhs}
\input{Darcs/Patch/V1/Show.lhs}
\input{Darcs/Patch/Show.lhs}

