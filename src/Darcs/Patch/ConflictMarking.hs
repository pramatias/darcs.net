--  Copyright (C) 2002-2003 David Roundy, 2010 Ganesh Sittampalam
{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.ConflictMarking
     ( mangleUnravelled
     ) where

import qualified Data.ByteString.Char8 as BC (pack, last)
import qualified Data.ByteString       as B (null, ByteString)
import Data.List ( sort, intersperse )
import Data.Maybe ( isJust )

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk, isHunk )
import Darcs.Patch.FileName ( FileName, fn2fp, fp2fn )
import Darcs.Patch.Inspect ( PatchInspect(..) )
import Darcs.Patch.Invert ( Invert(..) )
import Darcs.Patch.Permutations ()
import Darcs.Patch.Prim ( PrimPatch, is_filepatch, primIsHunk, primFromHunk )
import Darcs.Witnesses.Ordered ( FL(..) )
import Darcs.Witnesses.Sealed ( Sealed(..), mapSeal )

#include "gadts.h"
#include "impossible.h"

applyHunks :: IsHunk prim => [Maybe B.ByteString] -> FL prim C(x y) -> [Maybe B.ByteString]
applyHunks ms ((isHunk -> Just (FileHunk _ l o n)):>:ps) = applyHunks (rls l ms) ps
    where rls k _ | k <=0 = bug $ "bad hunk: start position <=0 (" ++ show k ++ ")"
          rls 1 mls = map Just n ++ drop (length o) mls
          rls i (ml:mls) = ml : rls (i-1) mls
          rls _ [] = bug "rls in applyHunks"
applyHunks ms NilFL = ms
applyHunks _ (_:>:_) = impossible

getAFilename :: PrimPatch prim => [Sealed (FL prim C(x))] -> FileName
getAFilename ((Sealed ((is_filepatch -> Just f):>:_)):_) = f
getAFilename _ = fp2fn ""

getOld :: PrimPatch prim => [Maybe B.ByteString] -> [Sealed (FL prim C(x))] -> [Maybe B.ByteString]
getOld mls (ps:pss) = getOld (getHunksOld mls ps) pss
getOld mls [] = mls

getHunksOld :: PrimPatch prim => [Maybe B.ByteString] -> Sealed (FL prim C(x))
              -> [Maybe B.ByteString]
getHunksOld mls (Sealed ps) =
    applyHunks (applyHunks mls ps) (invert ps)

getHunksNew :: IsHunk prim => [Maybe B.ByteString] -> Sealed (FL prim C(x))
              -> [Maybe B.ByteString]
getHunksNew mls (Sealed ps) = applyHunks mls ps

getHunkline :: [[Maybe B.ByteString]] -> Int
getHunkline = ghl 1
    where ghl :: Int -> [[Maybe B.ByteString]] -> Int
          ghl n pps =
            if any (isJust . head) pps
            then n
            else ghl (n+1) $ map tail pps

makeChunk :: Int -> [Maybe B.ByteString] -> [B.ByteString]
makeChunk n mls = pull_chunk $ drop (n-1) mls
    where pull_chunk (Just l:mls') = l : pull_chunk mls'
          pull_chunk (Nothing:_) = []
          pull_chunk [] = bug "should this be [] in pull_chunk?"


mangleUnravelled :: PrimPatch prim => [Sealed (FL prim C(x))] -> Sealed (FL prim C(x))
mangleUnravelled pss = if onlyHunks pss
                        then (:>: NilFL) `mapSeal` mangleUnravelledHunks pss
                        else head pss

onlyHunks :: forall prim C(x) . PrimPatch prim => [Sealed (FL prim C(x))] -> Bool
onlyHunks [] = False
onlyHunks pss = fn2fp f /= "" && all oh pss
    where f = getAFilename pss
          oh :: Sealed (FL prim C(y)) -> Bool
          oh (Sealed (p:>:ps)) = primIsHunk p &&
                                 [fn2fp f] == listTouchedFiles p &&
                                 oh (Sealed ps)
          oh (Sealed NilFL) = True

mangleUnravelledHunks :: PrimPatch prim => [Sealed (FL prim C(x))] -> Sealed (prim C(x))
--mangleUnravelledHunks [[h1],[h2]] = Deal with simple cases handily?
mangleUnravelledHunks pss =
        if null nchs then bug "mangleUnravelledHunks"
                     else Sealed (primFromHunk (FileHunk filename l old new))
    where oldf = getOld (repeat Nothing) pss
          newfs = map (getHunksNew oldf) pss
          l = getHunkline $ oldf : newfs
          nchs = sort $ map (makeChunk l) newfs
          filename = getAFilename pss
          old = makeChunk l oldf
          new = [top] ++ old ++ [initial] ++ concat (intersperse [middle] nchs) ++ [bottom]
          top    = BC.pack $ "v v v v v v v" ++ eol_c
          initial= BC.pack $ "=============" ++ eol_c
          middle = BC.pack $ "*************" ++ eol_c
          bottom = BC.pack $ "^ ^ ^ ^ ^ ^ ^" ++ eol_c
          eol_c  = if any (\ps -> not (B.null ps) && BC.last ps == '\r') old
                   then "\r"
                   else ""

