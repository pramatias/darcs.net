\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Apply () where

import Darcs.Patch.Apply ( Apply(..) )
import Darcs.Patch.Repair ( RepairToFL(..) )

import Darcs.Patch.Prim.Class ( PrimApply(..) )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..),
      DirPatchType(..), FilePatchType(..) )
import Darcs.Patch.Prim.V1.Show ( showHunk )

import Darcs.Patch.FileName ( fn2fp, fp2fn )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.TokenReplace ( tryTokInternal )

import Darcs.Global ( darcsdir )
import Darcs.IO ( WriteableDirectory(..), ReadableDirectory(..) )

import Darcs.Repository.Prefs ( changePrefval )

import Darcs.Witnesses.Ordered ( FL(..), mapFL_FL, spanFL, (:>)(..) )

import ByteStringUtils ( unlinesPS, breakAfterNthNewline, breakBeforeNthNewline, )
import Printer( renderString )

import Control.Monad ( when )
import qualified Data.ByteString as B ( ByteString, empty, null, concat )
import qualified Data.ByteString.Char8 as BC (pack, singleton, unpack)
import Data.List ( intersperse )

#include "gadts.h"
#include "impossible.h"

type FileContents = B.ByteString

instance Apply Prim where
    apply (FP f RmFile) = mRemoveFile f
    apply (FP f AddFile) = mCreateFile f
    apply p@(FP _ (Hunk _ _ _)) = applyPrimFL (p :>: NilFL)
    apply (FP f (TokReplace t o n)) = mModifyFilePSs f doreplace
        where doreplace ls =
                  case mapM (tryTokInternal t (BC.pack o) (BC.pack n)) ls of
                  Nothing -> fail $ "replace patch to " ++ fn2fp f
                             ++ " couldn't apply."
                  Just ls' -> return $ map B.concat ls'
    apply (FP f (Binary o n)) = mModifyFilePS f doapply
        where doapply oldf = if o == oldf
                             then return n
                             else fail $ "binary patch to " ++ fn2fp f
                                  ++ " couldn't apply."
    apply (DP d AddDir) = mCreateDirectory d
    apply (DP d RmDir) = mRemoveDirectory d
    apply (Move f f') = mRename f f'
    apply (ChangePref p f t) =
        do b <- mDoesDirectoryExist (fp2fn $ darcsdir++"/prefs")
           when b $ changePrefval p f t

instance RepairToFL Prim where
    applyAndTryToFixFL (FP f RmFile) =
        do x <- mReadFilePS f
           if B.null x then do mRemoveFile f
                               return Nothing
                       else do mWriteFilePS f B.empty
                               mRemoveFile f
                               return $ Just ("WARNING: Fixing removal of non-empty file "++fn2fp f,
                                              FP f (Binary x B.empty) :>: FP f RmFile :>: NilFL )
    applyAndTryToFixFL p = do apply p; return Nothing

instance PrimApply Prim where
    applyPrimFL NilFL = return ()
    applyPrimFL ((FP f h@(Hunk _ _ _)):>:the_ps)
     = case spanFL f_hunk the_ps of
           (xs :> ps') ->
               do let foo = h :>: mapFL_FL (\(FP _ h') -> h') xs
                  mModifyFilePS f $ hunkmod foo
                  applyPrimFL ps'
        where f_hunk (FP f' (Hunk _ _ _)) | f == f' = True
              f_hunk _ = False
              hunkmod :: WriteableDirectory m => FL FilePatchType C(x y)
                      -> B.ByteString -> m B.ByteString
              hunkmod NilFL ps = return ps
              hunkmod (Hunk line old new:>:hs) ps
               = case applyHunkLines [(line,old,new)] ps of
                     Just ps' -> hunkmod hs ps'
                     Nothing -> fail $ "### Error applying:\n" ++
                                       (renderString $ showHunk NewFormat f line old new) ++
                                       "\n### to file " ++ fn2fp f ++ ":\n" ++ BC.unpack ps
              hunkmod _ _ = impossible
    applyPrimFL (p:>:ps) = do apply p
                              applyPrimFL ps
\end{code}

\subsection{Hunk patches}

Hunks are an example of a complex filepatch.  A hunk is a set of lines of a
text file to be replaced by a different set of lines.  Either of these sets
may be empty, which would mean a deletion or insertion of lines.
\begin{code}
applyHunks :: [(Int, [B.ByteString], [B.ByteString])]
           -> B.ByteString -> Maybe [B.ByteString]
applyHunks [] ps = Just [ps]
applyHunks ((l, [], n):hs) ps
    = case breakBeforeNthNewline (l - 2) ps of
      (prfix, after_prefix) -> do rest <- applyHunks hs after_prefix
                                  return $ intersperse nl (prfix:n) ++ rest
                                       where nl = BC.singleton '\n'
applyHunks ((l, o, n):hs) ps
    = case breakBeforeNthNewline (l - 2) ps of
      (prfix, after_prefix) ->
          case breakBeforeNthNewline (length o) after_prefix of
          (oo, _) | oo /= unlinesPS (B.empty:o) -> fail "applyHunks error"
          (_, suffix) ->
              do rest <- applyHunks hs suffix
                 return $ intersperse nl (prfix:n) ++ rest
    where nl = BC.singleton '\n'

applyHunkLines :: [(Int, [B.ByteString], [B.ByteString])]
               -> FileContents -> Maybe FileContents
applyHunkLines [] c = Just c
applyHunkLines [(1, [], n)] ps | B.null ps = Just $ unlinesPS (n++[B.empty])
applyHunkLines hs@((l, o, n):hs') ps =
 do pss <- case l of
           1 -> case breakAfterNthNewline (length o) ps of
                Nothing -> if ps == unlinesPS o
                           then return $ intersperse nl n
                           else fail "applyHunkLines: Unexpected hunks"
                Just (shouldbeo, suffix)
                    | shouldbeo /= unlinesPS (o++[B.empty]) ->
                        fail $ "applyHunkLines: Bad patch!"
                    | null n ->
                        do x <- applyHunkLines hs' suffix
                           return [x]
                    | otherwise ->
                        do rest <- applyHunks hs' suffix
                           return $ intersperse nl n ++ nl:rest
           _ | l < 0 -> bug "Prim.applyHunkLines: After -ve lines?"
             | otherwise -> applyHunks hs ps
    let result = B.concat pss
    return result
    where nl = BC.singleton '\n'
\end{code}

\subsection{Token replace patches}\label{token_replace}

Although most filepatches will be hunks, darcs is clever enough to support
other types of changes as well.  A ``token replace'' patch replaces all
instances of a given token with some other version.  A token, here, is
defined by a regular expression, which must be of the simple [a--z\ldots]\ type,
indicating which characters are allowed in a token, with all other
characters acting as delimiters.  For example, a C identifier would be a
token with the flag \verb![A-Za-z_0-9]!.

What makes the token replace patch special is the fact that a token replace
can be merged with almost any ordinary hunk, giving exactly what you would
want.  For example, you might want to change the patch type {\tt
TokReplace} to {\tt TokenReplace} (if you decided that saving two
characters of space was stupid).  If you did this using hunks, it would
modify every line where {\tt TokReplace} occurred, and quite likely provoke
a conflict with another patch modifying those lines.  On the other hand, if
you did this using a token replace patch, the only change that it could
conflict with would be if someone else had used the token ``{\tt
TokenReplace}'' in their patch rather than TokReplace---and that actually
would be a real conflict!
