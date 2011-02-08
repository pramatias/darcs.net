-- Copyright (C) 2009 Petr Rockai
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

module Darcs.Diff( treeDiff ) where
import Darcs.Witnesses.Ordered ( FL(..), (+>+) )
import Darcs.Witnesses.Sealed ( Gap(..) )
import Darcs.Repository.Prefs ( FileType(..) )
import Darcs.Patch ( PrimPatch, hunk, canonize, binary
                   , addfile, rmfile, adddir, rmdir, invert)

import Storage.Hashed.Tree( diffTrees, zipTrees, TreeItem(..), Tree
                          , readBlob, emptyBlob )
import Storage.Hashed.AnchoredPath( AnchoredPath, anchorPath )

import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.List ( sortBy )
import ByteStringUtils( isFunky )

#include "gadts.h"
#include "impossible.h"

data Diff m = Added (TreeItem m) | Removed (TreeItem m) | Changed (TreeItem m) (TreeItem m)

getDiff :: AnchoredPath -> Maybe (TreeItem m) -> Maybe (TreeItem m) -> (AnchoredPath, Diff m)
getDiff p Nothing (Just t) = (p, Added t)
getDiff p (Just from) (Just to) = (p, Changed from to)
getDiff p (Just t) Nothing = (p, Removed t)
getDiff _ Nothing Nothing = impossible -- zipTrees should never return this

treeDiff :: forall m w prim . (Functor m, Monad m, Gap w, PrimPatch prim) => (FilePath -> FileType) -> Tree m -> Tree m -> m (w (FL prim))
treeDiff ft t1 t2 = do
  (from, to) <- diffTrees t1 t2
  diffs <- mapM (uncurry diff) $ sortBy organise $ zipTrees getDiff from to
  return $ foldr (joinGap (+>+)) (emptyGap NilFL) diffs
    where
          -- sort into removes, changes, adds, with removes in reverse-path order
          -- and everything else in forward order
          organise :: (AnchoredPath, Diff m) -> (AnchoredPath, Diff m) -> Ordering

          organise (p1, Changed _ _ ) (p2, Changed _ _) = compare p1 p2
          organise (p1, Added _)      (p2, Added _)   = compare p1 p2
          organise (p1, Removed _)    (p2, Removed _) = compare p2 p1

          organise (_, Removed _) _ = LT
          organise _ (_, Removed _) = GT

          organise (_, Changed _ _) _ = LT
          organise _ (_, Changed _ _) = GT

          diff :: AnchoredPath -> Diff m -> m (w (FL prim))
          diff _ (Changed (SubTree _) (SubTree _)) = return (emptyGap NilFL)
          diff p (Removed (SubTree _)) =
              return $ freeGap (rmdir (anchorPath "" p) :>: NilFL)
          diff p (Added (SubTree _)) =
              return $ freeGap (adddir (anchorPath "" p) :>: NilFL)
          diff p (Added b'@(File _)) =
              do diff' <- diff p (Changed (File emptyBlob) b')
                 return $ joinGap (:>:) (freeGap (addfile (anchorPath "" p))) diff'
          diff p (Removed a'@(File _)) =
              do diff' <- diff p (Changed a' (File emptyBlob))
                 return $ joinGap (+>+) diff' (freeGap (rmfile (anchorPath "" p) :>: NilFL))
          diff p (Changed (File a') (File b')) =
              do a <- readBlob a'
                 b <- readBlob b'
                 let path = anchorPath "" p
                 case ft path of
                   TextFile | no_bin a && no_bin b ->
                                return $ text_diff path a b
                   _ -> return $ if a /= b
                                    then freeGap (binary path (strict a) (strict b) :>: NilFL)
                                    else emptyGap NilFL
          diff p _ = fail $ "Missing case at path " ++ show p
          text_diff p a b
              | BL.null a && BL.null b = emptyGap NilFL
              | BL.null a = freeGap (diff_from_empty p b)
              | BL.null b = freeGap (diff_to_empty p a)
              | otherwise = freeGap (line_diff p (linesB a) (linesB b))
          line_diff p a b = canonize (hunk p 1 a b)
          diff_to_empty p x | BLC.last x == '\n' = line_diff p (init $ linesB x) []
                            | otherwise = line_diff p (linesB x) [BS.empty]
          diff_from_empty p x = invert (diff_to_empty p x)
          no_bin = not . isFunky . strict . BL.take 4096
          linesB = map strict . BLC.split '\n'
          strict = BS.concat . BL.toChunks

