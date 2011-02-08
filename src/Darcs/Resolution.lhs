%  Copyright (C) 2003,2005 David Roundy
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

#include "gadts.h"

module Darcs.Resolution ( standardResolution,
                          externalResolution,
                          patchsetConflictResolutions,
                        ) where

import System.FilePath.Posix ( (</>) )
import System.Exit ( ExitCode( ExitSuccess ) )
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Data.List ( zip4 )
import Control.Monad ( when )

import Darcs.Diff( treeDiff )
import Darcs.Patch ( PrimOf, PrimPatch, RepoPatch, joinPatches, resolveConflicts,
                     applyToFilepaths, patchcontents,
                     invert, listConflictedFiles, commute, applyToTree )
import Darcs.RepoPath ( toFilePath )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (+>+),
                             mapFL_FL, reverseRL )

import CommandLine ( parseCmd )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Utils ( askUser, filterFilePaths )
import Darcs.Patch.Set ( PatchSet(..) )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Sealed ( Sealed(..), unFreeLeft )
import Darcs.Repository.Prefs ( filetypeFunction )
import Exec ( exec, Redirect(..) )
import Darcs.Lock ( withTempDir )
import Darcs.External ( cloneTree )

import qualified Storage.Hashed.Tree as Tree
import Storage.Hashed ( writePlainTree, readPlainTree )

--import Darcs.ColorPrinter ( traceDoc )
--import Printer ( greenText, ($$), Doc )
--import Darcs.Patch ( showPatch )

standardResolution :: RepoPatch p => FL p C(x y) -> Sealed (FL (PrimOf p) C(y))
standardResolution p = mergeList $ map head $ resolveConflicts p

mergeList :: forall prim C(x) . PrimPatch prim => [Sealed (FL prim C(x))] -> Sealed (FL prim C(x))
mergeList patches = doml NilFL patches
    where doml :: FL prim C(x y) -> [Sealed (FL prim C(x))] -> Sealed (FL prim C(x))
          doml mp (Sealed p:ps) =
              case commute (invert p :> mp) of
              Just (mp' :> _) -> doml (p +>+ mp') ps
              Nothing -> doml mp ps -- This shouldn't happen for "good" resolutions.
          doml mp [] = Sealed mp
\end{code}

\paragraph{Resolution of conflicts}\label{resolution}

To resolve conflicts using an external tool, you need to specify a command
to use, e.g.
\begin{verbatim}
--external-merge 'opendiff %1 %2 -ancestor %a -merge %o'
\end{verbatim}
The \verb!%1! and \verb!%2!  are replaced with the two versions to be
merged, \verb!%a! is replaced with the common ancestor of the two versions.
Most importantly, \verb!%o! is replaced with the name of the output file
that darcs will require to be created holding the merged version.  The
above example works with the FileMerge.app tool that comes with Apple's
developer tools.  To use xxdiff, you would use
\begin{verbatim}
--external-merge 'xxdiff -m -O -M %o %1 %a %2'
\end{verbatim}
To use \verb!kdiff3!, you can use
\begin{verbatim}
--external-merge 'kdiff3 --output %o %a %1 %2'
\end{verbatim}
To use \verb!tortoiseMerge!, you can use
\begin{verbatim}
--external-merge 'tortoiseMerge /base:"%a" /mine:"%1" /theirs:"%2" /merged:"%o"'
\end{verbatim}
(\verb!tortoiseMerge! is a nice merge tool that comes with TortoiseSVN and works well
on Windows.)

% Fixme: Is it actually a shell command on MS Windows?
Note that the command is split into space-separated words and the first one is
\verb!exec!ed with the rest as arguments---it is not a shell command. In particular,
on Windows this means that the first command path should not contain spaces and
you should make sure the command is in your \verb!PATH!.

The substitution of the \verb!%! escapes is done everywhere. If you need to prevent
substitution you can use a double percentage sign, i.e. \verb!%%a! is substituted with
\verb!%a!. Here is an example script to use the Emacs' Ediff package for merging.
% This is indented so that the leading #s don't confuse the preprocessor.
\begin{verbatim}
 #! /bin/sh
 # External merge command for darcs, using Emacs Ediff, via server if possible.
 # It needs args %1 %2 %a %o, i.e. the external merge command is, say,
 # `emerge3 %1 %2 %a %o'.
 test $# -eq 4 || exit 1
 form="(ediff-merge-files-with-ancestor"
 while test $# -gt 0; do
     count=$count.
     if [ $count = .... ]; then
         form=$form\ nil         # Lisp STARTUP-HOOKS arg
     fi
     case $1 in                  # Worry about quoting -- escape " and \
         *[\"\\]* ) form=$form\ \"$(echo $1 | sed -e's/["\\]/\\\0/g')\" ;;
         *) form=$form\ \"$1\" ;;
     esac
     shift
 done
 form=$form')'
 ( emacsclient --eval "$form" || # Emacs 22 server
   gnudoit "$form" ||            # XEmacs/Emacs 21 server
   emacs --eval "$form" ||       # Relatively slow to start up
   xemacs -eval "$form"          # Horribly slow to start up
 ) 2>/dev/null
\end{verbatim}
It would be invoked like:
\begin{verbatim}
--external-merge 'emerge3 %1 %2 %a %o'
\end{verbatim}

If you figure out how to use darcs with another merge tool, please let me
know what flags you used so I can mention it here.

Note that if you do use an external merge tool, most likely you will want
to add to your defaults file
(\verb!_darcs/prefs/defaults! or \verb!~/.darcs/prefs!, see \ref{defaults},
on MS Windows~\ref{ms_win})
a line such as
\begin{verbatim}
ALL external-merge kdiff3 --output %o %a %1 %2
\end{verbatim}
or
\begin{verbatim}
ALL external-merge tortoiseMerge /base:"%a" /mine:"%1" /theirs:"%2" /merged:"%o"
\end{verbatim}

Note that the defaults file does not want quotes around the command.

\begin{code}
externalResolution :: RepoPatch p => Tree.Tree IO -> String -> FL (PrimOf p) C(x y) -> FL (PrimOf p) C(x z)
                    -> FL p C(y a)
                    -> IO (Sealed (FL (PrimOf p) C(a)))
externalResolution s1 c p1 p2 pmerged = do
 sa <- applyToTree (invert p1) s1
 sm <- applyToTree pmerged s1
 s2 <- applyToTree p2 sa
 let nms = listConflictedFiles pmerged
     nas = applyToFilepaths (invert pmerged) nms
     n1s = applyToFilepaths p1 nas
     n2s = applyToFilepaths p2 nas
     ns = zip4 nas n1s n2s nms
     write_files tree fs = writePlainTree (Tree.filter (filterFilePaths fs) tree) "."
  in do
   former_dir <- getCurrentDirectory
   withTempDir "version1" $ \absd1 -> do
     let d1 = toFilePath absd1
     write_files s1 n1s
     setCurrentDirectory former_dir
     withTempDir "ancestor" $ \absda -> do
       let da = toFilePath absda
       write_files sa nas
       setCurrentDirectory former_dir
       withTempDir "merged" $ \absdm -> do
         let dm = toFilePath absdm
         write_files sm nms
         setCurrentDirectory former_dir
         withTempDir "cleanmerged" $ \absdc -> do
           let dc = toFilePath absdc
           cloneTree dm "."
           setCurrentDirectory former_dir
           withTempDir "version2" $ \absd2 -> do
             let d2 = toFilePath absd2
             write_files s2 n2s
             mapM_ (externallyResolveFile c da d1 d2 dm) ns
             sc <- readPlainTree dc
             sfixed <- readPlainTree dm
             ftf <- filetypeFunction
             unFreeLeft `fmap` treeDiff ftf sc sfixed

externallyResolveFile :: String -> String -> String -> String -> String
                        -> (FilePath, FilePath, FilePath, FilePath)
                        -> IO ()
externallyResolveFile c da d1 d2 dm (fa, f1, f2, fm) = do
    putStrLn $ "Merging file "++fm++" by hand."
    ec <- run c [('1', d1</>f1), ('2', d2</>f2), ('a', da</>fa), ('o', dm</>fm), ('%', "%")]
    when (ec /= ExitSuccess) $
         putStrLn $ "External merge command exited with " ++ show ec
-- Warning:  A do-notation statement discarded a result of type String.
    _ <- askUser "Hit return to move on, ^C to abort the whole operation..."
    return ()

run :: String -> [(Char,String)] -> IO ExitCode
run c replacements =
    case parseCmd replacements c of
    Left err     -> fail $ show err
    Right (c2,_) -> rr c2
    where rr (command:args) = do putStrLn $ "Running command '" ++
                                            unwords (command:args) ++ "'"
                                 exec command args (Null,Null,Null)
          rr [] = return ExitSuccess

patchsetConflictResolutions :: RepoPatch p => PatchSet p C(Origin x) -> Sealed (FL (PrimOf p) C(x))
patchsetConflictResolutions (PatchSet NilRL _) = Sealed NilFL
patchsetConflictResolutions (PatchSet xs _)
    = --traceDoc (greenText "looking at resolutions" $$
      --         (sh $ resolveConflicts $ joinPatches $
      --              mapFL_FL (patchcontents . hopefully) $ reverseRL xs )) $
      mergeList $ map head $ resolveConflicts $ joinPatches $
      mapFL_FL (patchcontents . hopefully) $ reverseRL xs
    --where sh :: [[Sealed (FL Prim)]] -> Doc
    --      sh [] = greenText "no more conflicts"
    --      sh (x:ps) = greenText "one conflict" $$ sh1 x $$ sh ps
    --      sh1 :: [Sealed (FL Prim)] -> Doc
    --      sh1 [] = greenText "end of unravellings"
    --      sh1 (Sealed x:ps) = greenText "one unravelling:" $$ showPatch x $$
    --                          sh1 ps
\end{code}
