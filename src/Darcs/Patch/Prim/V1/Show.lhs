\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ViewPatterns #-}
module Darcs.Patch.Prim.V1.Show
    ( showHunk )
    where

import Prelude hiding ( pi )

import ByteStringUtils ( fromPS2Hex )
import qualified Data.ByteString as B (ByteString, length, take, drop)
import qualified Data.ByteString.Char8 as BC (head)

import Darcs.Patch.FileHunk ( FileHunk(..), IsHunk(..), showFileHunk )
import Darcs.Patch.FileName ( FileName )
import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Show ( ShowPatchBasic(..), ShowPatch(..), formatFileName )
import Darcs.Patch.Summary ( plainSummaryPrim, plainSummaryPrims )
import Darcs.Patch.Viewing ( showContextHunk )
import Darcs.Patch.Permutations () -- for Invert instance of FL
import Darcs.Patch.Prim.Class ( PrimShow(..) )
import Darcs.Patch.Prim.V1.Core
     ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.Prim.V1.Details ()
import Darcs.Witnesses.Show ( appPrec, Show1(..), Show2(..), ShowDict(..) )
import Printer ( Doc, vcat,
                 text, userchunk, invisibleText, invisiblePS, blueText,
                 ($$), (<+>), (<>),
               )

#include "gadts.h"

-- TODO this instance shouldn't really be necessary, as Prims aren't used generically
instance PatchListFormat Prim

instance ShowPatchBasic Prim where
    showPatch = showPrim OldFormat

instance ShowPatch Prim where
    showContextPatch (isHunk -> Just fh) = showContextHunk fh
    showContextPatch p = return $ showPatch p
    summary = plainSummaryPrim
    summaryFL = plainSummaryPrims
    thing _ = "change"

instance Show (Prim C(x y)) where
    showsPrec d (Move fn1 fn2) = showParen (d > appPrec) $ showString "Move " .
                                 showsPrec (appPrec + 1) fn1 . showString " " .
                                 showsPrec (appPrec + 1) fn2
    showsPrec d (DP fn dp) = showParen (d > appPrec) $ showString "DP " .
                             showsPrec (appPrec + 1) fn . showString " " .
                             showsPrec (appPrec + 1) dp
    showsPrec d (FP fn fp) = showParen (d > appPrec) $ showString "FP " .
                             showsPrec (appPrec + 1) fn . showString " " .
                             showsPrec (appPrec + 1) fp
    showsPrec d (ChangePref p f t) = showParen (d > appPrec) $ showString "ChangePref " .
                                     showsPrec (appPrec + 1) p . showString " " .
                                     showsPrec (appPrec + 1) f . showString " " .
                                     showsPrec (appPrec + 1) t

instance Show2 Prim where
   showDict2 = ShowDictClass

instance Show1 (Prim C(x)) where
   showDict1 = ShowDictClass

instance Show (FilePatchType C(x y)) where
    showsPrec _ RmFile = showString "RmFile"
    showsPrec _ AddFile = showString "AddFile"
    showsPrec d (Hunk line old new) | all ((==1) . B.length) old && all ((==1) . B.length) new
        = showParen (d > appPrec) $ showString "Hunk " .
                                      showsPrec (appPrec + 1) line . showString " " .
                                      showsPrecC old . showString " " .
                                      showsPrecC new
       where showsPrecC [] = showString "[]"
             showsPrecC ss = showParen True $ showString "packStringLetters " . showsPrec (appPrec + 1) (map BC.head ss)
    showsPrec d (Hunk line old new) = showParen (d > appPrec) $ showString "Hunk " .
                                      showsPrec (appPrec + 1) line . showString " " .
                                      showsPrec (appPrec + 1) old . showString " " .
                                      showsPrec (appPrec + 1) new
    showsPrec d (TokReplace t old new) = showParen (d > appPrec) $ showString "TokReplace " .
                                         showsPrec (appPrec + 1) t . showString " " .
                                         showsPrec (appPrec + 1) old . showString " " .
                                         showsPrec (appPrec + 1) new
    -- this case may not work usefully
    showsPrec d (Binary old new) = showParen (d > appPrec) $ showString "Binary " .
                                   showsPrec (appPrec + 1) old . showString " " .
                                   showsPrec (appPrec + 1) new

instance Show (DirPatchType C(x y)) where
    showsPrec _ RmDir = showString "RmDir"
    showsPrec _ AddDir = showString "AddDir"

{-
instance Show (Prim C(x y))  where
    show p = renderString (showPrim p) ++ "\n"
-}

instance PrimShow Prim where
  showPrim x (FP f AddFile) = showAddFile x f
  showPrim x (FP f RmFile)  = showRmFile x f
  showPrim x (FP f (Hunk line old new))  = showHunk x f line old new
  showPrim x (FP f (TokReplace t old new))  = showTok x f t old new
  showPrim x (FP f (Binary old new))  = showBinary x f old new
  showPrim x (DP d AddDir) = showAddDir x d
  showPrim x (DP d RmDir)  = showRmDir x d
  showPrim x (Move f f') = showMove x f f'
  showPrim _ (ChangePref p f t) = showChangePref p f t

\end{code}


\paragraph{Add file}
Add an empty file to the tree.

\verb!addfile filename!
\begin{code}
showAddFile :: FileNameFormat -> FileName -> Doc
showAddFile x f = blueText "addfile" <+> formatFileName x f
\end{code}

\paragraph{Remove file}
Delete a file from the tree.

\verb!rmfile filename!
\begin{code}
showRmFile :: FileNameFormat -> FileName -> Doc
showRmFile x f = blueText "rmfile" <+> formatFileName x f
\end{code}

\paragraph{Move}
Rename a file or directory.

\verb!move oldname newname!
\begin{code}
showMove :: FileNameFormat -> FileName -> FileName -> Doc
showMove x d d' = blueText "move" <+> formatFileName x d <+> formatFileName x d'
\end{code}

\paragraph{Change Pref}
Change one of the preference settings.  Darcs stores a number of simple
string settings.  Among these are the name of the test script and the name
of the script that must be called prior to packing in a make dist.
\begin{verbatim}
changepref prefname
oldval
newval
\end{verbatim}
\begin{code}
showChangePref :: String -> String -> String -> Doc
showChangePref p f t = blueText "changepref" <+> text p
                    $$ userchunk f
                    $$ userchunk t
\end{code}

\paragraph{Add dir}
Add an empty directory to the tree.

\verb!adddir filename!
\begin{code}
showAddDir :: FileNameFormat -> FileName -> Doc
showAddDir x d = blueText "adddir" <+> formatFileName x d
\end{code}

\paragraph{Remove dir}
Delete a directory from the tree.

\verb!rmdir filename!
\begin{code}
showRmDir :: FileNameFormat -> FileName -> Doc
showRmDir x d = blueText "rmdir" <+> formatFileName x d
\end{code}


\paragraph{Hunk}
Replace a hunk (set of contiguous lines) of text with a new
hunk.
\begin{verbatim}
hunk FILE LINE#
-LINE
...
+LINE
...
\end{verbatim}
\begin{code}
showHunk :: FileNameFormat -> FileName -> Int -> [B.ByteString] -> [B.ByteString] -> Doc
showHunk x f line old new = showFileHunk x (FileHunk f line old new)
\end{code}

\paragraph{Token replace}

Replace a token with a new token.  Note that this format means that
whitespace must not be allowed within a token.  If you know of a practical
application of whitespace within a token, let me know and I may change
this.
\begin{verbatim}
replace FILENAME [REGEX] OLD NEW
\end{verbatim}
\begin{code}
showTok :: FileNameFormat -> FileName -> String -> String -> String -> Doc
showTok x f t o n = blueText "replace" <+> formatFileName x f
                                     <+> text "[" <> userchunk t <> text "]"
                                     <+> userchunk o
                                     <+> userchunk n
\end{code}

\paragraph{Binary file modification}

Modify a binary file
\begin{verbatim}
binary FILENAME
oldhex
*HEXHEXHEX
...
newhex
*HEXHEXHEX
...
\end{verbatim}
\begin{code}
showBinary :: FileNameFormat -> FileName -> B.ByteString -> B.ByteString -> Doc
showBinary x f o n =
    blueText "binary" <+> formatFileName x f
 $$ invisibleText "oldhex"
 $$ (vcat $ map makeprintable $ breakEvery 78 $ fromPS2Hex o)
 $$ invisibleText "newhex"
 $$ (vcat $ map makeprintable $ breakEvery 78 $ fromPS2Hex n)
     where makeprintable ps = invisibleText "*" <> invisiblePS ps

breakEvery :: Int -> B.ByteString -> [B.ByteString]
breakEvery n ps | B.length ps < n = [ps]
                 | otherwise = B.take n ps : breakEvery n (B.drop n ps)

\end{code}