module Darcs.Patch.Summary
    ( plainSummary, plainSummaryPrim, plainSummaryPrims,
      xmlSummary )
    where

import Darcs.Patch.FileName ( fn2fp )
import Darcs.Patch.Conflict ( Conflict(..), IsConflictedPrim(IsC), ConflictState(..) )
import Darcs.Patch.Effect ( Effect )
import Darcs.Patch.Prim.Class ( PrimDetails(..), PrimPatchBase )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Witnesses.Ordered ( FL, mapFL )

import Printer ( Doc, empty, vcat,
                 text,
                 minus, plus, ($$), (<+>), (<>),
               )

#include "gadts.h"

plainSummaryPrim :: PrimDetails prim => prim C(x y) -> Doc
plainSummaryPrim = vcat . map summChunkToLine . genSummary . (:[]) . IsC Okay

plainSummaryPrims :: PrimDetails prim => FL prim C(x y) -> Doc
plainSummaryPrims = vcat . map summChunkToLine . genSummary . mapFL (IsC Okay)

plainSummary :: (Conflict e, Effect e, PrimPatchBase e) => e C(x y) -> Doc
plainSummary = vcat . map summChunkToLine . genSummary . conflictedEffect

xmlSummary :: (Effect p, Conflict p, PrimPatchBase p) => p C(x y) -> Doc
xmlSummary p = text "<summary>"
             $$ (vcat . map summChunkToXML . genSummary . conflictedEffect $ p)
             $$ text "</summary>"

-- Yuck duplicated code below...
escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ (strReplace x y zs)
  | otherwise = z : (strReplace x y zs)
-- end yuck duplicated code.

-- | High-level representation of a piece of patch summary
data SummChunk = SummChunk SummDetail ConflictState
   deriving (Ord, Eq)

genSummary :: forall p . PrimDetails p => [IsConflictedPrim p] -> [SummChunk]
genSummary p
    = combine $ concatMap s2 p
    where s2 :: IsConflictedPrim p -> [SummChunk]
          s2 (IsC c x) = map (\d -> SummChunk d c) $ summarizePrim x
          combine (x1@(SummChunk d1 c1) : x2@(SummChunk d2 c2) : ss)
              = case combineDetail d1 d2 of
                  Nothing -> x1 : combine (x2:ss)
                  Just d3 -> combine $ SummChunk d3 (combineConflitStates c1 c2) : ss
          combine (x:ss) = x  : combine ss
          combine [] = []
          --
          combineDetail (SummFile o1 f1 r1 a1 x1) (SummFile o2 f2 r2 a2 x2) | f1 == f2 =
            do o3 <- combineOp o1 o2
               return $ SummFile o3 f1 (r1 + r2) (a1 + a2) (x1 + x2)
          combineDetail _ _ = Nothing
          --
          combineConflitStates Conflicted _ = Conflicted
          combineConflitStates _ Conflicted = Conflicted
          combineConflitStates Duplicated _ = Duplicated
          combineConflitStates _ Duplicated = Duplicated
          combineConflitStates Okay Okay = Okay
          -- Don't combine AddFile and RmFile: (maybe an old revision of) darcs
          -- allows a single patch to add and remove the same file, see issue 185
          combineOp SummAdd SummRm  = Nothing
          combineOp SummRm  SummAdd = Nothing
          combineOp SummAdd _ = Just SummAdd
          combineOp _ SummAdd = Just SummAdd
          combineOp SummRm  _ = Just SummRm
          combineOp _ SummRm  = Just SummRm
          combineOp SummMod SummMod = Just SummMod

summChunkToXML :: SummChunk -> Doc
summChunkToXML (SummChunk detail c) =
 case detail of
   SummRmDir f  -> xconf c "remove_directory" (xfn f)
   SummAddDir f -> xconf c "add_directory"    (xfn f)
   SummFile SummRm  f _ _ _ -> xconf c "remove_file" (xfn f)
   SummFile SummAdd f _ _ _ -> xconf c "add_file"    (xfn f)
   SummFile SummMod f r a x -> xconf c "modify_file" $ xfn f <> xrm r <> xad a <> xrp x
   SummMv f1 f2  -> text "<move from=\"" <> xfn f1
                      <> text "\" to=\"" <> xfn f2 <> text"\"/>"
   SummNone      -> empty
 where
   xconf Okay t x       = text ('<':t++">") $$ x $$ text ("</"++t++">")
   xconf Conflicted t x = text ('<':t++" conflict='true'>") $$ x $$ text ("</"++t++">")
   xconf Duplicated t x = text ('<':t++" duplicate='true'>") $$ x $$ text ("</"++t++">")
   xfn = escapeXML . dropDotSlash .fn2fp
   --
   xad 0 = empty
   xad a = text "<added_lines num='" <> text (show a) <> text "'/>"
   xrm 0 = empty
   xrm a = text "<removed_lines num='" <> text (show a) <> text "'/>"
   xrp 0 = empty
   xrp a = text "<replaced_tokens num='" <> text (show a) <> text "'/>"

summChunkToLine :: SummChunk -> Doc
summChunkToLine (SummChunk detail c) =
  case detail of
   SummRmDir f   -> lconf c "R" $ text (fn2fp f) <> text "/"
   SummAddDir f  -> lconf c "A" $ text (fn2fp f) <> text "/"
   SummFile SummRm  f _ _ _ -> lconf c "R" $ text (fn2fp f)
   SummFile SummAdd f _ _ _ -> lconf c "A" $ text (fn2fp f)
   SummFile SummMod f r a x -> lconf c "M" $ text (fn2fp f) <+> rm r <+> ad a <+> rp x
   SummMv f1 f2 -> text " "    <> text (fn2fp f1)
                <> text " -> " <> text (fn2fp f2)
   SummNone -> case c of
               Okay -> empty
               _    -> lconf c ""  empty
  where
   lconf Okay       t x = text t <+> x
   lconf Conflicted t x = text (t ++ "!") <+> x
   lconf Duplicated t x = text t <+> x <+> text "duplicate"
   --
   ad 0 = empty
   ad a = plus <> text (show a)
   rm 0 = empty
   rm a = minus <> text (show a)
   rp 0 = empty
   rp a = text "r" <> text (show a)

dropDotSlash :: FilePath -> FilePath
dropDotSlash ('.':'/':str) = dropDotSlash str
dropDotSlash str = str

