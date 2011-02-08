module Darcs.Patch.FileHunk
    ( FileHunk(..), IsHunk(..), showFileHunk
    )
    where

import Darcs.Patch.FileName ( FileName )
import Darcs.Patch.Format ( FileNameFormat )
import Darcs.Patch.Show ( formatFileName )

import Printer
    ( Doc, blueText, text, lineColor, vcat, userchunkPS
    , prefix, ($$), (<+>), Color(Cyan, Magenta) )

import qualified Data.ByteString as B ( ByteString )

#include "gadts.h"

data FileHunk C(x y) = FileHunk !FileName !Int [B.ByteString] [B.ByteString]

class IsHunk p where
    isHunk :: p C(x y) -> Maybe (FileHunk C(x y))

showFileHunk :: FileNameFormat -> FileHunk C(x y) -> Doc
showFileHunk x (FileHunk f line old new) =
           blueText "hunk" <+> formatFileName x f <+> text (show line)
        $$ lineColor Magenta (prefix "-" (vcat $ map userchunkPS old))
        $$ lineColor Cyan    (prefix "+" (vcat $ map userchunkPS new))
