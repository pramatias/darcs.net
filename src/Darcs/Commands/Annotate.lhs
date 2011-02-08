%  Copyright (C) 2003 David Roundy
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

\darcsCommand{annotate}
\begin{code}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Commands.Annotate ( annotate, createdAsXml ) where

import Control.Monad ( when )
import Data.List ( sort )
import Control.Applicative ( (<$>) )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir,
                         summary, unified, humanReadable,
                        xmloutput, creatorhash,
                        maybeFixSubPaths,
                        listRegisteredFiles,
                        matchOne,
                      )
import Darcs.Flags ( isUnified )
import Storage.Hashed.Plain( readPlainTree )
import Darcs.Repository ( Repository, amInRepository, withRepository, RepoJob(..), readRepo,
                          getMarkedupFile )
import Darcs.Patch.Set ( PatchSet, newset2RL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Patch ( RepoPatch, Named, LineMark(..), patch2patchinfo, xmlSummary )
import qualified Darcs.Patch ( summary )
import Darcs.Witnesses.Ordered ( mapRL )
import qualified Data.ByteString.Char8 as BC ( unpack, ByteString )
import Darcs.PrintPatch ( printPatch, contextualPrintPatch )
import Darcs.Patch.Info ( PatchInfo, humanFriendly, toXml, makeFilename,
                   showPatchInfo )
import Darcs.Patch.PopulationData ( Population(..), PopTree(..), DirMark(..),
                        nameI, modifiedByI, modifiedHowI,
                        createdByI, creationNameI,
                      )
import Darcs.Patch.Population ( getRepoPopVersion, lookupPop, lookupCreationPop,
                    modifiedToXml,
                  )
import Darcs.Patch.PatchInfoAnd ( info )
import Darcs.RepoPath ( SubPath, toFilePath )
import Darcs.Match ( matchPatch, haveNonrangeMatch, getFirstMatch )
import Darcs.Lock ( withTempDir )
import Darcs.Witnesses.Sealed ( Sealed2(..), unseal2 )
import Printer ( putDocLn, text, errorDoc, ($$), prefix, (<+>),
                 Doc, empty, vcat, (<>), renderString, packedString )
#include "impossible.h"

annotateDescription :: String
annotateDescription = "Display which patch last modified something."

annotateHelp :: String
annotateHelp =
 "The `darcs annotate' command provides two unrelated operations.  When\n" ++
 "called on a file, it will find the patch that last modified each line\n" ++
 "in that file.  When called on a patch (e.g. using --patch), it will\n" ++
 "print the internal representation of that patch.\n" ++
 "\n" ++
 "The --summary option will result in a summarized patch annotation,\n" ++
 "similar to `darcs whatsnew'.  It has no effect on file annotations.\n" ++
 "\n" ++
 "By default, output is in a human-readable format.  The --xml-output\n" ++
 "option can be used to generate output for machine postprocessing.\n"

annotate :: DarcsCommand
annotate = DarcsCommand {commandProgramName = "darcs",
                         commandName = "annotate",
                         commandHelp = annotateHelp,
                         commandDescription = annotateDescription,
                         commandExtraArgs = -1,
                         commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                         commandCommand = annotateCmd,
                         commandPrereq = amInRepository,
                         commandGetArgPossibilities = listRegisteredFiles,
                         commandArgdefaults = nodefaults,
                         commandAdvancedOptions = [],
                         commandBasicOptions = [summary,unified,
                                                 humanReadable,
                                                 xmloutput,
                                                 matchOne, creatorhash,
                                                 workingRepoDir]}

\end{code}
%% FIXME: AFAICT -u does nothing.  Remove it from anno's options?
%% --twb, 2009-09-20
Giving the \verb!--unified! flag implies \verb!--human-readable!, and causes
the output to remain in a darcs-specific format that is similar to that produced
by \verb!diff --unified!.
\begin{code}
annotateCmd :: [DarcsFlag] -> [String] -> IO ()
annotateCmd opts args = case args of
  [] -> if haveNonrangeMatch opts
    then annotatePattern opts
    else fail $ "Annotate requires either a patch pattern or a " ++
      "file or directory argument."
  [""] -> annotateCmd opts []
  [_] -> do
    f <- head <$> maybeFixSubPaths opts args
    case f of
      Nothing -> fail "invalid argument"
      Just f' -> annotatePath opts f'
  _ -> fail "annotate accepts at most one argument"

annotatePattern :: [DarcsFlag] -> IO ()
annotatePattern opts =
  withRepository opts $ RepoJob $ \repository -> do
  Sealed2 p <- matchPatch opts `fmap` readRepo repository
  if Summary `elem` opts
     then do putDocLn $ showpi $ patch2patchinfo p
             putDocLn $ show_summary p
     else if isUnified opts
          then withTempDir "context" $ \_ ->
               do getFirstMatch repository opts
                  c <- readPlainTree "."
                  contextualPrintPatch c p
          else printPatch p
    where showpi | MachineReadable `elem` opts = showPatchInfo
                 | XMLOutput `elem` opts       = toXml
                 | otherwise                   = humanFriendly
          show_summary :: RepoPatch p => Named p C(x y) -> Doc
          show_summary = if XMLOutput `elem` opts
                         then xmlSummary
                         else Darcs.Patch.summary
\end{code}

If a directory name is given, annotate will output details of the last
modifying patch for each file in the directory and the directory itself. The
details look like this:

\begin{verbatim}
 # Created by [bounce handling patch
 # mark**20040526202216]  as ./test/m7/bounce_handling.pl
    bounce_handling.pl
\end{verbatim}

If a patch name and a directory are given, these details are output for the time after
that patch was applied.  If a directory and a tag name are given, the
details of the patches involved in the specified tagged version will be output.
\begin{code}
annotatePath :: [DarcsFlag] -> SubPath -> IO ()
annotatePath opts file = withRepository opts $ RepoJob $ \repository -> do
  r <- readRepo repository
  pinfo <- if haveNonrangeMatch opts
           then return $ patch2patchinfo `unseal2` (matchPatch opts r)
           else case mapRL info $ newset2RL r of
                [] -> fail "Annotate does not currently work correctly on empty repositories."
                (x:_) -> return x
  pop <- getRepoPopVersion "." pinfo

  -- deal with --creator-hash option
  let maybe_creation_pi = findCreationPatchinfo opts r
      lookup_thing = case maybe_creation_pi of
                     Nothing -> lookupPop
                     Just cp -> lookupCreationPop cp
  let file' = toFilePath file
  if null file'
    then case pop of (Pop _ pt) -> annotatePop opts pinfo pt
    else case lookup_thing file' pop of
      Nothing -> fail $ "There is no file or directory named '"++file'++"'"
      Just (Pop _ pt@(PopDir i _))
          | modifiedHowI i == RemovedDir && modifiedByI i /= pinfo ->
              errorDoc $ text ("The directory '" ++ file' ++
                               "' was removed by")
                      $$ humanFriendly (modifiedByI i)
          | otherwise -> annotatePop opts pinfo pt
      Just (Pop _ pt@(PopFile i))
          | modifiedHowI i == RemovedFile && modifiedByI i /= pinfo ->
              errorDoc $ text ("The file '" ++ file' ++
                               "' was removed by")
                      $$ humanFriendly (modifiedByI i)
          | otherwise -> annotateFile repository opts pinfo file pt

annotatePop :: [DarcsFlag] -> PatchInfo -> PopTree PatchInfo -> IO ()
annotatePop opts pinfo pt = putDocLn $ p2format pinfo pt
    where p2format = if XMLOutput `elem` opts
                     then p2xml
                     else p2s

indent :: Doc -> [Doc]
-- This is a bit nasty:
indent = map (text . i) . lines . renderString
    where i "" = ""
          i ('#':s) = ('#':s)
          i s = "    "++s

-- Annotate a directory listing
p2s :: PatchInfo -> PopTree PatchInfo -> Doc
p2s pinfo (PopFile inf) =
    created_str
 $$ f <+> file_change
    where f = packedString $ nameI inf
          file_created = text "Created by"
                     <+> showPatchInfo (fromJust $ createdByI inf)
                     <+> text "as"
                     <+> packedString (fromJust $ creationNameI inf)
          created_str = prefix "# " file_created
          file_change = if modifiedByI inf == pinfo
                        then text $ show (modifiedHowI inf)
                        else empty
p2s pinfo (PopDir inf pops) =
    created_str
 $$ dir <+> dir_change
 $$ vcat (map (vcat . indent . p2s pinfo) $ sort pops)
    where dir = packedString (nameI inf) <> text "/"
          dir_created =
              if createdByI inf /= Nothing
              then text "Created by "
               <+> showPatchInfo (fromJust $ createdByI inf)
               <+> text "as"
               <+> packedString (fromJust $ creationNameI inf) <> text "/"
              else text "Root directory"
          created_str = prefix "# " dir_created
          dir_change = if modifiedByI inf == pinfo
                       then text $ show (modifiedHowI inf)
                       else empty

escapeXML :: String -> Doc
escapeXML = text . strReplace '\'' "&apos;" . strReplace '"' "&quot;" .
  strReplace '>' "&gt;" . strReplace '<' "&lt;" . strReplace '&' "&amp;"

strReplace :: Char -> String -> String -> String
strReplace _ _ [] = []
strReplace x y (z:zs)
  | x == z    = y ++ (strReplace x y zs)
  | otherwise = z : (strReplace x y zs)

createdAsXml :: PatchInfo -> String -> Doc
createdAsXml pinfo as = text "<created_as original_name='"
                       <> escapeXML as
                       <> text "'>"
                    $$    toXml pinfo
                    $$    text "</created_as>"
--removed_by_xml :: PatchInfo -> String
--removed_by_xml pinfo = "<removed_by>\n"++toXml pinfo++"</removed_by>\n"

p2xmlOpen :: PatchInfo -> PopTree PatchInfo -> Doc
p2xmlOpen _ (PopFile inf) =
    text "<file name='" <> escapeXML f <> text "'>"
 $$ created
 $$ modified
    where f = BC.unpack $ nameI inf
          created = case createdByI inf of
                    Nothing -> empty
                    Just ci -> createdAsXml ci
                               (BC.unpack $ fromJust $ creationNameI inf)
          modified = modifiedToXml inf
p2xmlOpen _ (PopDir inf _) =
    text "<directory name='" <> escapeXML f <> text "'>"
 $$ created
 $$ modified
    where f = BC.unpack $ nameI inf
          created = case createdByI inf of
                    Nothing -> empty
                    Just ci -> createdAsXml ci
                               (BC.unpack $ fromJust $ creationNameI inf)
          modified = modifiedToXml inf

p2xmlClose :: PatchInfo -> PopTree PatchInfo -> Doc
p2xmlClose _(PopFile _) = text "</file>"
p2xmlClose _ (PopDir _ _) = text "</directory>"

p2xml :: PatchInfo -> PopTree PatchInfo -> Doc
p2xml pinf p@(PopFile _) = p2xmlOpen pinf p $$ p2xmlClose pinf p
p2xml pinf p@(PopDir _ pops) = p2xmlOpen pinf p
                            $$ vcat (map (p2xml pinf) $ sort pops)
                            $$ p2xmlClose pinf p
\end{code}

If a file name is given, the last modifying patch details of that file will be output, along
with markup indicating patch details when each line was last (and perhaps next) modified.

If a patch name and a file name are given, these details are output for the time after
that patch was applied.

\begin{code}
annotateFile :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> PatchInfo -> SubPath -> PopTree PatchInfo -> IO ()
annotateFile repository opts pinfo f (PopFile inf) = do
  if XMLOutput `elem` opts
     then putDocLn $ p2xmlOpen pinfo (PopFile inf)
     else if createdByI inf /= Nothing
          then putAnn $ text ("File "++toFilePath f++" created by ")
                     <> showPatchInfo ci <> text (" as " ++ createdname)
          else putAnn $ text $ "File "++toFilePath f
  mk <- getMarkedupFile repository ci createdname
  old_pis <- (dropWhile (/= pinfo).mapRL info.newset2RL) `fmap` readRepo repository
  mapM_ (annotateMarkedup opts pinfo old_pis) mk
  when (XMLOutput `elem` opts) $  putDocLn $ p2xmlClose pinfo (PopFile inf)
  where ci = fromJust $ createdByI inf
        createdname = BC.unpack $ fromJust $ creationNameI inf
annotateFile _ _ _ _ _ = impossible

annotateMarkedup :: [DarcsFlag] -> PatchInfo -> [PatchInfo]
                  -> (BC.ByteString, LineMark PatchInfo) -> IO ()
annotateMarkedup opts | XMLOutput `elem` opts = xmlMarkedup
                       | otherwise = textMarkedup

textMarkedup :: PatchInfo -> [PatchInfo] -> (BC.ByteString, LineMark PatchInfo) -> IO ()
textMarkedup _ _ (l,None) = putLine ' ' l
textMarkedup pinfo old_pis (l,RemovedLine wheni)
    | wheni == pinfo       = putLine '-' l
    | wheni `elem` old_pis = return ()
    | otherwise            = putLine ' ' l
textMarkedup pinfo old_pis (l,AddedLine wheni)
    | wheni == pinfo       = putLine '+' l
    | wheni `elem` old_pis = do putAnn $ text "Following line added by "
                                      <> showPatchInfo wheni
                                putLine ' ' l
    | otherwise            = return ()
textMarkedup pinfo old_pis (l,AddedRemovedLine whenadd whenrem)
    | whenadd == pinfo = do putAnn $ text "Following line removed by "
                                  <> showPatchInfo whenrem
                            putLine '+' l
    | whenrem == pinfo = do putAnn $ text "Following line added by "
                                  <> showPatchInfo whenadd
                            putLine '-' l
    | whenadd `elem` old_pis && not (whenrem `elem` old_pis) =
        do putAnn $ text "Following line removed by " <> showPatchInfo whenrem
           putAnn $ text "Following line added by " <> showPatchInfo whenadd
           putLine ' ' l
    | otherwise = return ()

putLine :: Char -> BC.ByteString -> IO ()
putLine c s = putStrLn $ c : BC.unpack s
putAnn :: Doc -> IO ()
putAnn s = putDocLn $ prefix "# " s

xmlMarkedup :: PatchInfo -> [PatchInfo] -> (BC.ByteString, LineMark PatchInfo) -> IO ()
xmlMarkedup _ _ (l,None) = putLine ' ' l
xmlMarkedup pinfo old_pis (l,RemovedLine wheni)
    | wheni == pinfo       = putDocLn $ text "<removed_line>"
                             $$ escapeXML (BC.unpack l)
                             $$ text "</removed_line>"
    | wheni `elem` old_pis = return ()
    | otherwise            = putDocLn $ text "<normal_line>"
                             $$ text "<removed_by>"
                             $$ toXml wheni
                             $$ text "</removed_by>"
                             $$ escapeXML (BC.unpack l)
                             $$ text "</normal_line>"
xmlMarkedup pinfo old_pis (l,AddedLine wheni)
    | wheni == pinfo       = putDocLn $ text "<added_line>"
                             $$ escapeXML (BC.unpack l)
                             $$ text "</added_line>"
    | wheni `elem` old_pis = putDocLn $ text "<normal_line>"
                             $$ text "<added_by>"
                             $$ toXml wheni
                             $$ text "</added_by>"
                             $$ escapeXML (BC.unpack l)
                             $$ text "</normal_line>"
    | otherwise            = return ()
xmlMarkedup pinfo old_pis (l,AddedRemovedLine whenadd whenrem)
    | whenadd == pinfo =
        putDocLn $ text "<added_line>"
                $$ text "<removed_by>"
                $$ toXml whenrem
                $$ text "</removed_by>"
                $$ escapeXML (BC.unpack l)
                $$ text "</added_line>"
    | whenrem == pinfo =
        putDocLn $ text "<removed_line>"
                $$ text "<added_by>"
                $$ toXml whenadd
                $$ text "</added_by>"
                $$ escapeXML (BC.unpack l)
                $$ text "</removed_line>"
    | whenadd `elem` old_pis && not (whenrem `elem` old_pis) =
        putDocLn $ text "<normal_line>"
                $$ text "<removed_by>"
                $$ toXml whenrem
                $$ text "</removed_by>"
                $$ text "<added_by>"
                $$ toXml whenadd
                $$ text "</added_by>"
                $$ escapeXML (BC.unpack l)
                $$ text "</normal_line>"
    | otherwise = return ()
\end{code}

\begin{options}
--creator-hash HASH
\end{options}

The \verb!--creator-hash! option should only be used in combination with a
file or directory to be annotated.  In this case, the name of that file or
directory is interpreted to be its name \emph{at the time it was created},
and the hash given along with \verb!--creator-hash! indicates the patch
that created the file or directory.  This allows you to (relatively) easily
examine a file even if it has been renamed multiple times.

\begin{code}
findCreationPatchinfo :: [DarcsFlag] -> PatchSet p C(Origin x) -> Maybe PatchInfo
findCreationPatchinfo [] _ = Nothing
findCreationPatchinfo (CreatorHash h:_) r = findHash h $ mapRL info $ newset2RL r
findCreationPatchinfo (_:fs) r = findCreationPatchinfo fs r

findHash :: String -> [PatchInfo] -> Maybe PatchInfo
findHash _ [] = Nothing
findHash h (pinf:pinfs)
    | take (length h) (makeFilename pinf) == h = Just pinf
    | otherwise = findHash h pinfs
\end{code}
