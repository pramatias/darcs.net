{-# LANGUAGE CPP, RankNTypes, GADTs, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- Copyright (C) 2009 Ganesh Sittampalam
--
-- Permission is hereby granted, free of charge, to any person
-- obtaining a copy of this software and associated documentation
-- files (the "Software"), to deal in the Software without
-- restriction, including without limitation the rights to use, copy,
-- modify, merge, publish, distribute, sublicense, and/or sell copies
-- of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be
-- included in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
-- BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
-- SOFTWARE.

module Darcs.Patch.Split ( Splitter(..), rawSplitter, noSplitter, primSplitter, reversePrimSplitter ) where

import Data.List ( intersperse )

import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..) )
import Darcs.Patch.Patchy ( ReadPatch(..), showPatch, ShowPatch(..), Invert(..) )
import Darcs.Patch.Invert (invertFL)
import Darcs.Patch.Prim ( PrimPatch, canonize, canonizeFL, primFromHunk )
import Darcs.Patch.ReadMonads ( parseStrictly )
import Darcs.Patch.Read ()
import Darcs.Patch.Viewing ()

import Printer ( renderPS )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

#include "gadts.h"

-- |A splitter is something that can take a patch and (possibly) render it
-- as text in some format of its own choosing.
-- This text can then be presented to the user for editing, and the result
-- given to the splitter for parsing.
-- If the parse succeeds, the result is a list of patches that could replace
-- the original patch in any context.
-- Typically this list will contain the changed version of the patch, along
-- with fixup pieces to ensure that the overall effect of the list is the same
-- as the original patch.
-- The individual elements of the list can then be offered separately to the
-- user, allowing them to accept some and reject others.
--
-- There's no immediate application for a splitter for anything other than
-- Prim (you shouldn't go editing named patches, you'll break them!)
-- However you might want to compose splitters for FilePatchType to make
-- splitters for Prim etc, and the generality doesn't cost anything.
data Splitter p
  = Splitter {
              applySplitter :: FORALL(x y) p C(x y)
                              -> Maybe (B.ByteString,
                                        B.ByteString -> Maybe (FL p C(x y)))
              -- canonization is needed to undo the effects of splitting
              -- Typically, the list returned by applySplitter will not
              -- be in the simplest possible form (since the user will have
              -- deliberately added extra stuff). Once the user has selected
              -- the pieces they want, we need to make sure that we eliminate
              -- any remaining redundancy in the selected pieces, otherwise
              -- we might record (or whatever) a rather strange looking patch.
              -- This hook allows the splitter to provide an appropriate
              -- function for doing this.
             ,canonizeSplit :: FORALL(x y) FL p C(x y) -> FL p C(x y)
             }

{- Some facts that probably ought to be true about splitters: should make some QC
properties

applySplitter p = Just (bs, f) ==> f bs == Just (p :>: NilFL)

applySplitter p = Just (bs, f) ; f bs' = Just ps ==> canonizeSplit ps = p :>: NilFL

-}

-- Does not canonize as there is no generic operation to do this.
withEditedHead :: Invert p => p C(x y) -> p C(x z) -> FL p C(x y)
withEditedHead p res = res :>: invert res :>: p :>: NilFL

-- |This generic splitter just lets the user edit the printed representation of the patch
-- Should not be used expect for testing and experimentation.
rawSplitter :: (ShowPatch p, ReadPatch p, Invert p) => Splitter p
rawSplitter = Splitter {
                  applySplitter =
                     \p -> Just (renderPS . showPatch $ p,
                                 \str -> case parseStrictly readPatch' str of
                                          Just (Sealed res, _) -> Just (withEditedHead p res)
                                          _ -> Nothing
                                )
                 ,canonizeSplit = id
                }

-- |Never splits. In other code we normally pass around Maybe Splitter instead of using this
-- as the default, because it saves clients that don't care about splitting from having to
-- import this module just to get noSplitter.
noSplitter :: Splitter p
noSplitter = Splitter { applySplitter = const Nothing, canonizeSplit = id }


doPrimSplit :: PrimPatch prim => prim C(x y) -> Maybe (B.ByteString, B.ByteString -> Maybe (FL prim C(x y)))
doPrimSplit (isHunk -> Just (FileHunk fn n before after))
 = Just (B.concat $ intersperse (BC.pack "\n") $ concat
           [ helptext
           , [mkSep " BEFORE (reference) =========================="]
           , before
           , [mkSep "=== AFTER (edit) ============================="]
           , after
           , [mkSep "=== (edit above) ============================="]
           ],
         \bs -> do let ls = BC.split '\n' bs
                   (_, ls2) <- breakSep ls        -- before
                   (before', ls3) <- breakSep ls2 -- after 1
                   (after', _) <- breakSep ls3    -- after 2
                   return (hunk before before' +>+ hunk before' after' +>+ hunk after' after))
    where sep = BC.pack "=========================="
          helptext = map BC.pack [ "Interactive hunk edit:"
                                 , " - Edit the section marked 'AFTER'"
                                 , " - Arbitrary editing is supported"
                                 , " - This will only affect the patch, not your working copy"
                                 , " - Hints:"
                                 , "   - To split added text, delete the part you want to postpone"
                                 , "   - To split removed text, copy back the part you want to retain"
                                 , ""
                                 ]
          hunk b a = canonize (primFromHunk (FileHunk fn n b a))
          mkSep s = BC.append sep (BC.pack s)
          breakSep xs = case break (sep `BC.isPrefixOf`) xs of
                           (_, []) -> Nothing
                           (ys, _:zs) -> Just (ys, zs)
doPrimSplit _ = Nothing

-- |Split a primitive hunk patch up
-- by allowing the user to edit both the before and after lines, then insert fixup patches
-- to clean up the mess.
primSplitter :: PrimPatch p => Splitter p
primSplitter  = Splitter { applySplitter = doPrimSplit
                         , canonizeSplit = canonizeFL }

doReversePrimSplit :: PrimPatch prim => prim C(x y) -> Maybe (B.ByteString, B.ByteString -> Maybe (FL prim C(x y)))
doReversePrimSplit prim = do
  (text, parser) <- doPrimSplit (invert prim)
  let parser' p = do
        patch <- parser  p
        return . reverseRL $ invertFL patch
  return (text, parser')

reversePrimSplitter :: PrimPatch prim => Splitter prim
reversePrimSplitter = Splitter { applySplitter = doReversePrimSplit
                               , canonizeSplit = canonizeFL}
