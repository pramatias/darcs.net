\begin{code}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Details
    ()
    where

import Prelude hiding ( pi )
import Darcs.Patch.Prim.Class ( PrimDetails(..) )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..), FilePatchType(..), DirPatchType(..) )
import Darcs.Patch.MarkupData ( LineMark(..), MarkedUpFile )
import Darcs.Patch.PopulationData ( Population(..), PopTree(..), Info(..), DirMark(..) )
import Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) )
import Darcs.Patch.TokenReplace ( tryTokInternal )
import Darcs.Patch.FileName ( fn2fp, fp2fn, movedirfilename, fn2ps )

import qualified Data.ByteString as B ( ByteString, concat )
import qualified Data.ByteString.Char8 as BC ( pack, split )
import Data.Maybe ( catMaybes )

#include "gadts.h"

instance PrimDetails Prim where
  summarizePrim (FP f (Hunk _ o n)) = [SummFile SummMod f (length o) (length n) 0]
  summarizePrim (FP f (Binary _ _)) = [SummFile SummMod f 0 0 0]
  summarizePrim (FP f AddFile) = [SummFile SummAdd f 0 0 0]
  summarizePrim (FP f RmFile) = [SummFile SummRm f 0 0 0]
  summarizePrim (FP f (TokReplace _ _ _)) = [SummFile SummMod f 0 0 1]
  summarizePrim (DP d AddDir) = [SummAddDir d]
  summarizePrim (DP d RmDir) = [SummRmDir d]
  summarizePrim (Move f1 f2) = [SummMv f1 f2]
  summarizePrim (ChangePref _ _ _) = [SummNone]
\end{code}

%\section{Outputting interesting and useful information}

%Just being able to manipulate patches and trees is not enough.  We also
%want to be able to view the patches and files.  This requires another set
%of functions, closely related to the patch application functions, which
%will give us the necessary information to browse the changes we have made.
%It is \emph{not} the Patch module's responsibility to add any sort of
%markup or formatting, but simply to provide the information necessary for an
%external module to do the formatting.

\begin{code}
  markupPrim _ (FP _ AddFile) (f, mk) = (f, mk)
  markupPrim _ (FP _ RmFile) (f, mk) = (f, mk)
  markupPrim n (FP f' (Hunk line old new)) (f, mk)
      | fn2fp f' /= f = (f, mk)
      | otherwise = (f, markupHunk n line old new mk)
  markupPrim name (FP f' (TokReplace t o n)) (f, mk)
      | fn2fp f' /= f = (f, mk)
      | otherwise = (f, markupTok name t o n mk)
  markupPrim _ (DP _ _) (f, mk) = (f, mk)
  markupPrim _ (Move d d') (f, mk) = (fn2fp $ movedirfilename d d' (fp2fn f), mk)
  markupPrim _ (ChangePref _ _ _) (f,mk) = (f,mk)
  markupPrim n (FP f' (Binary _ _)) (f,mk)
      | fn2fp f' == f = (f,(BC.pack "Binary file", AddedLine n):mk)
      | otherwise = (f,mk)

\end{code}

%apply a patch to a population at a given time

\begin{code}
  applyToPopPrim pi patch (Pop _ tree)
   = Pop pi (applyToPopTree patch tree)
     -- ``pi'' is global below!
   where
         applyToPopTree p@(FP f AddFile) tr =
             let xxx = BC.split '/' (fn2ps  f) in
                 popChange xxx p $ fst $ breakP xxx tr
         applyToPopTree p@(FP f _) tr = popChange (BC.split '/' (fn2ps  f)) p tr
         applyToPopTree p@(DP f AddDir) tr =
             let xxx = BC.split '/' (fn2ps  f) in
                 popChange xxx p $ fst $ breakP xxx tr
         applyToPopTree p@(DP d _) tr = popChange (BC.split '/' (fn2ps  d)) p tr
         -- precondition: ``to'' does not exist yet!
         applyToPopTree (Move from to) tr
          = case breakP (BC.split '/' (fn2ps from)) $
                 fst $ breakP (BC.split '/' $ fn2ps to) tr of
             (tr',Just ins) ->
                 let to' = (BC.split '/' (fn2ps to))
                     ins' = case ins of
                            PopDir i trs -> PopDir (i {nameI = last to',
                                                       modifiedByI = pi,
                                                       modifiedHowI = MovedDir (fn2fp from)})
                                                   trs
                            PopFile i -> PopFile (i {nameI = last to',
                                                     modifiedByI = pi,
                                                     modifiedHowI = MovedFile (fn2fp from)})
                               in insertP to' tr' ins'
             _ -> tr -- ignore the move if ``from'' couldn't be found
         applyToPopTree (ChangePref _ _ _) tr = tr

         -- insert snd arg into fst arg
         insertP [parent,_] org@(PopDir f trs) tr
          | parent == (nameI f) = PopDir f (tr:trs)
          | otherwise = org
         insertP (n:rest) org@(PopDir f trs) tr
          | (nameI f) == n = PopDir f trs'
          | otherwise = org
            where trs' = map (\o -> insertP rest o tr) trs
         insertP _ org _ = org

         -- change a population according to a patch
         popChange [parent,path] (DP d AddDir) tr@(PopDir f trs)
          | parent == (nameI f) = PopDir f (new:trs)
          | otherwise = tr
                where new = PopDir (Info {nameI = path,
                                          modifiedByI = pi,
                                          modifiedHowI = AddedDir,
                                          createdByI = Just pi,
                                          creationNameI = Just $ fn2ps d}) []
         -- only mark a directory (and contents) as ``deleted'' do not delete it actually
         popChange [path] (DP _ RmDir) tr@(PopDir f trs)
          | path == (nameI f) = PopDir (f {modifiedByI = pi,
                                           modifiedHowI = RemovedDir}) trs'
          | otherwise = tr
            where trs' = map markDel trs -- recursively ``delete'' the contents

         popChange [parent,path] (FP d AddFile) tr@(PopDir f trs)
          | parent == (nameI f) = PopDir f (new:trs)
          | otherwise = tr
                where new = PopFile (Info {nameI = path,
                                           modifiedByI = pi,
                                           modifiedHowI = AddedFile,
                                           createdByI = Just pi,
                                           creationNameI = Just $ fn2ps d})
         popChange [path] (FP _ RmFile) tr@(PopFile f)
          | path == (nameI f) = PopFile (f {modifiedByI = pi,
                                           modifiedHowI = RemovedFile})
          | otherwise = tr
         popChange [path] (FP _ _) (PopFile f)
          | path == (nameI f)
             = PopFile (f {modifiedByI = pi,
                           modifiedHowI = if modifiedHowI f == AddedFile && modifiedByI f == pi
                                          then AddedFile
                                          else ModifiedFile})
         popChange (n:rest) p tr@(PopDir f trs)
          | (nameI f) == n = PopDir f (map (popChange rest p) trs)
          | otherwise = tr
         popChange _ _ tr = tr
         markDel (PopDir f trs) = PopDir (f {modifiedByI = pi,
                                             modifiedHowI = RemovedDir}) trs'
                  where trs' = map markDel trs
         markDel (PopFile f) = PopFile (f {modifiedByI = pi,
                                           modifiedHowI = RemovedFile})
\end{code}

\begin{code}
markupHunk :: pi -> Int -> [B.ByteString] -> [B.ByteString]
            -> MarkedUpFile pi -> MarkedUpFile pi
markupHunk n l old new ((sf, RemovedLine pi):mk) =
    (sf, RemovedLine pi) : markupHunk n l old new mk
markupHunk n l old new ((sf, AddedRemovedLine po pn):mk) =
    (sf, AddedRemovedLine po pn) : markupHunk n l old new mk

markupHunk name 1 old (n:ns) mk =
    (n, AddedLine name) : markupHunk name 1 old ns mk
markupHunk n 1 (o:os) [] ((sf, None):mk)
    | o == sf = (sf, RemovedLine n) : markupHunk n 1 os [] mk
    | otherwise = [(BC.pack "Error in patch application", AddedLine n)]
markupHunk n 1 (o:os) [] ((sf, AddedLine nold):mk)
    | o == sf = (sf, AddedRemovedLine nold n) : markupHunk n 1 os [] mk
    | otherwise = [(BC.pack "Error in patch application", AddedLine n)]
markupHunk _ 1 [] [] mk = mk

markupHunk n l old new ((sf, AddedLine pi):mk)
    | l > 1 = (sf, AddedLine pi) : markupHunk n (l-1) old new mk
    | l < 1 = (sf, AddedLine pi) : markupHunk n (l-1) old new mk
markupHunk n l old new ((sf, None):mk)
    | l > 1 = (sf, None) : markupHunk n (l-1) old new mk
    | l < 1 = (sf, None) : markupHunk n (l-1) old new mk

markupHunk _ _ _ _ [] = []

markupHunk _ _ _ _ mk = (BC.pack "Error: ",None) : mk

markupTok :: pi -> String -> String -> String
           -> MarkedUpFile pi -> MarkedUpFile pi
markupTok name t ostr nstr mk = concatMap mt mk
    where o = BC.pack ostr
          n = BC.pack nstr
          mt (sf, AddedLine pi) =
              case B.concat `fmap` tryTokInternal t o n sf of
              Just sf' | sf' == sf -> [(sf, AddedLine pi)]
                       | otherwise -> [(sf, AddedRemovedLine pi name),
                                       (sf', AddedLine name)]
              Nothing ->
                  [(sf, AddedLine pi),
                   (BC.pack "There seems to be an inconsistency...", None),
                   (BC.pack "Please run darcs check.", None)]
          mt mark = [mark]

-- break a poptree fst: org tree with subtree removed,
--                 snd: removed subtree
breakP :: [B.ByteString] -> PopTree pi -> (PopTree pi,Maybe (PopTree pi))
breakP [parent,path] tr@(PopDir f trees)
 | parent == (nameI f) = case findRem path trees of
                         Just (trees',tree') -> (PopDir f trees',Just tree')
                         _ -> (tr,Nothing)
 | otherwise = (tr,Nothing)
 where findRem _ [] = Nothing
       findRem the_path (d:trs)
        | the_path == pname d = Just (trs,d)
        | otherwise = do (trs',d') <- findRem the_path trs
                         return (d:trs',d')
breakP (n:rest) tr@(PopDir f trs)
 | (nameI f) == n = case catMaybes inss of
                    [ins] -> (PopDir f trs', Just ins)
                    [] -> (tr,Nothing)
                    _ -> error "breakP: more than one break"
 | otherwise = (tr,Nothing)
   where (trs',inss) = unzip (map (breakP rest) trs)
breakP _ tr = (tr,Nothing)

pname :: PopTree pi -> B.ByteString
pname (PopDir i _) = nameI i
pname (PopFile i) = nameI i

\end{code}

