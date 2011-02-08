%  Copyright (C) 2003-2004 David Roundy
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

\darcsCommand{changes}
\begin{code}
{-# LANGUAGE CPP, PatternGuards #-}

#include "gadts.h"

module Darcs.Commands.Changes ( changes, log ) where
import Prelude hiding ( log )
import Unsafe.Coerce (unsafeCoerce)

import Data.List ( intersect, sort, nub )
import Data.Maybe ( fromMaybe, fromJust, isJust )
import Control.Monad ( when, unless )
import Control.Applicative ((<$>))

import Darcs.Patch.PatchInfoAnd ( hopefullyM, info )
import Darcs.Commands ( DarcsCommand(..), nodefaults, commandAlias )
import Darcs.Arguments ( DarcsFlag(GenContext, HumanReadable, MachineReadable,
                                   Interactive, Count,
                                   NumberPatches, XMLOutput, Summary,
                                   Verbose, Debug),
                         fixSubPaths, changesFormat,
                         possiblyRemoteRepoDir, getRepourl,
                         workingRepoDir, onlyToFiles,
                         summary, changesReverse,
                         matchSeveralOrRange,
                         matchMaxcount, maxCount,
                         allInteractive, showFriendly,
                         networkOptions
                      )
import Darcs.Flags ( doReverse, showChangesOnlyToFiles, UseIndex(..), ScanKnown(..) )
import Darcs.RepoPath ( SubPath(), toFilePath )
import Darcs.Patch.FileName ( fp2fn, fn2fp, normPath )
import Darcs.Repository ( PatchSet, PatchInfoAnd,
                          withRepositoryDirectory, RepoJob(..), findRepository,
                          readRepo, unrecordedChanges )
import Darcs.Patch.Set ( PatchSet(..), newset2RL )
import Darcs.Patch.Info ( toXml, showPatchInfo )
import Darcs.Patch.Depends ( findCommonWithThem )
import Darcs.Patch.Bundle( contextPatches )
import Darcs.Patch.TouchesFiles ( lookTouch )
import Darcs.Patch ( RepoPatch, invert, xmlSummary, description, applyToFilepaths,
                     listTouchedFiles, effect )
import Darcs.Witnesses.Eq ( EqCheck(..) )
import Darcs.Witnesses.Ordered ( FL(NilFL), RL(..), filterFLFL, filterRL,
                                 reverseFL, (:>)(..), mapRL )
import Darcs.Match ( firstMatch, secondMatch,
               matchAPatchread, haveNonrangeMatch,
               matchFirstPatchset, matchSecondPatchset,
             )
import Darcs.Commands.Annotate ( createdAsXml )
import Printer ( Doc, putDocLnWith, simplePrinters, (<+>),
                 renderString, prefix, text, vcat, vsep,
                 ($$), empty, errorDoc, insertBeforeLastline )
import Darcs.ColorPrinter ( fancyPrinters )
import Progress ( setProgressMode, debugMessage )
import Darcs.SelectChanges ( viewChanges )
import Darcs.Witnesses.Sealed ( Sealed2(..), unseal2, Sealed(..), seal2 )

changesDescription :: String
changesDescription = "List patches in the repository."

changesHelp :: String
changesHelp =
 "The `darcs changes' command lists the patches that constitute the\n" ++
 "current repository or, with --repo, a remote repository.  Without\n" ++
 "options or arguments, ALL patches will be listed.\n" ++
 "\n" ++ changesHelp' ++
 "\n" ++ changesHelp''

changes :: DarcsCommand
changes = DarcsCommand {commandProgramName = "darcs",
                        commandName = "changes",
                        commandHelp = changesHelp,
                        commandDescription = changesDescription,
                        commandExtraArgs = -1,
                        commandExtraArgHelp = ["[FILE or DIRECTORY]..."],
                        commandGetArgPossibilities = return [],
                        commandCommand = changesCmd,
                        commandPrereq = findRepository,
                        commandArgdefaults = nodefaults,
                        commandAdvancedOptions = networkOptions,
                        commandBasicOptions = [matchSeveralOrRange,
                                                 matchMaxcount,
                                                 onlyToFiles,
                                                 changesFormat,
                                                 summary,
                                                 changesReverse,
                                                 possiblyRemoteRepoDir,
                                                 workingRepoDir,
                                                 allInteractive]}

changesCmd :: [DarcsFlag] -> [String] -> IO ()
changesCmd opts args
  | GenContext `elem` opts = if not . null $ args
      then fail "changes --context cannot accept other arguments"
      else changesContext opts
  | null args = showChanges opts Nothing
  | otherwise = do
      fs <- fixSubPaths opts args
      case fs of
        [] -> putStrLn "No valid arguments were given, nothing to do."
        _ -> showChanges opts . Just . nub $ sort fs

showChanges :: [DarcsFlag] -> Maybe [SubPath] -> IO ()
showChanges opts files =
  let repodir = fromMaybe "." (getRepourl opts) in
  withRepositoryDirectory opts repodir $ RepoJob $ \repository -> do
  unless (Debug `elem` opts) $ setProgressMode False
  Sealed unrec <- case files of
    Nothing -> return $ Sealed NilFL
    Just _ -> Sealed `fmap` unrecordedChanges (UseIndex, ScanKnown) repository files
                  `catch` \_ -> return (Sealed NilFL) -- this is triggered when repository is remote
  let filez = map (fn2fp . normPath . fp2fn) . applyToFilepaths (invert unrec) . map toFilePath <$> files
      filtered_changes p = maybe_reverse $ getChangesInfo opts filez p
  debugMessage "About to read the repository..."
  patches <- readRepo repository
  debugMessage "Done reading the repository."
  if Interactive `elem` opts
    then do let (fp_and_fs, _, _) = filtered_changes patches
                fp = map fst fp_and_fs
            viewChanges opts fp
    else do when (isJust files && not (XMLOutput `elem` opts)) $
                 putStrLn $ "Changes to "++unwords (fromJust filez)++":\n"
            debugMessage "About to print the changes..."
            let printers = if XMLOutput `elem` opts then simplePrinters else fancyPrinters
            ps <- readRepo repository -- read repo again to prevent holding onto
                                       -- values forced by filtered_changes
            putDocLnWith printers $ changelog opts ps $ filtered_changes patches
  where maybe_reverse (xs,b,c) = if doReverse opts
                                 then (reverse xs, b, c)
                                 else (xs, b, c)


-- FIXME: this prose is unreadable. --twb, 2009-08
changesHelp' :: String
changesHelp' =
 "When given one or more files or directories as arguments, only\n" ++
 "patches which affect those files or directories are listed. This\n" ++
 "includes changes that happened to files before they were moved or\n" ++
 "renamed.\n" ++
 "\n" ++
 "When given a --from-tag, --from-patch or --from-match, only changes\n" ++
 "since that tag or patch are listed.  Similarly, the --to-tag,\n" ++
 "--to-patch and --to-match options restrict the list to older patches.\n" ++
 "\n" ++
 "The --last and --max-count options both limit the number of patches\n" ++
 "listed.  The former applies BEFORE other filters, whereas the latter\n" ++
 "applies AFTER other filters.  For example `darcs changes foo.c\n" ++
 "--max-count 3' will print the last three patches that affect foo.c,\n" ++
 "whereas `darcs changes --last 3 foo.c' will, of the last three\n" ++
 "patches, print only those that affect foo.c.\n"

getChangesInfo :: RepoPatch p => [DarcsFlag] -> Maybe [FilePath]
               -> PatchSet p C(x y)
               -> ([(Sealed2 (PatchInfoAnd p), [FilePath])], [FilePath], Doc)
getChangesInfo opts plain_fs ps =
    case (sp1s, sp2s) of
      (Sealed p1s, Sealed p2s) ->
          case findCommonWithThem p2s p1s of
            _ :> us ->
              let ps' = filterRL pf (reverseFL us) in
                case plain_fs of
                  Nothing -> foldr (\x xs -> (x, []) -:- xs) ([], [], empty) $
                    maybe id take (maxCount opts) ps'
                  Just fs -> let fs' = map (\x -> "./" ++ x) fs in
                    filterPatchesByNames (maxCount opts) fs' ps'
  where sp1s = if firstMatch opts
               then matchFirstPatchset opts ps
               else Sealed $ PatchSet NilRL NilRL
        sp2s = if secondMatch opts
               then matchSecondPatchset opts ps
               else Sealed $ ps
        pf = if haveNonrangeMatch opts
             then matchAPatchread opts
             else \_ -> True

-- | Take a list of filenames and patches and produce a list of
-- patches that actually touch the given files with list of touched
-- file names, a new file list that represents the same set of files
-- as in input, before the returned patches would have been applied,
-- and possibly an error. Additionaly, the function takes a "depth
-- limit" -- maxcount, that could be Nothing (return everything) or
-- "Just n" -- returns at most n patches touching the file (starting
-- from the beginning of the patch list).
filterPatchesByNames :: RepoPatch p =>
                           Maybe Int -- ^ maxcount
                        -> [FilePath] -- ^ filenames
                        -> [Sealed2 (PatchInfoAnd p)] -- ^ patchlist
                        -> ([(Sealed2 (PatchInfoAnd p),[FilePath])], [FilePath], Doc)
filterPatchesByNames (Just 0) _ _ = ([], [], empty)
filterPatchesByNames _ [] _ = ([], [], empty)
filterPatchesByNames _ _ [] = ([], [], empty)
filterPatchesByNames maxcount fs ((Sealed2 hp):ps)
    | Just p <- hopefullyM hp =
    case lookTouch fs (invert p) of
    (True, []) -> ([(Sealed2 hp, fs)], fs, empty)
    (True, fs') -> (Sealed2 hp, fs) -:- filterPatchesByNames
                                         (subtract 1 `fmap` maxcount) fs' ps
    (False, fs') -> filterPatchesByNames maxcount fs' ps
filterPatchesByNames _ _ ((Sealed2 hp):_) =
    ([], [], text "Can't find changes prior to:" $$ description hp)

-- | Note, lazy pattern matching is required to make functions like
-- filterPatchesByNames lazy in case you are only not interested in
-- the first element. E.g.:
--
--   let (fs, _, _) = filterPatchesByNames ...
(-:-) :: a -> ([a],b,c) -> ([a],b,c)
x -:- ~(xs,y,z) = (x:xs,y,z)

changelog :: forall p C(start x)
           . RepoPatch p
          => [DarcsFlag] -> PatchSet p C(start x)
          -> ([(Sealed2 (PatchInfoAnd p), [FilePath])], [FilePath], Doc)
          -> Doc
changelog opts patchset (pis_and_fs, orig_fs, errstring)
    | Count `elem` opts = text $ show $ length pis_and_fs
    | MachineReadable `elem` opts =
        if renderString errstring == ""
        then vsep $ map (unseal2 (showPatchInfo.info)) pis
        else errorDoc errstring
    | XMLOutput `elem` opts =
         text "<changelog>"
      $$ vcat xml_file_names
      $$ vcat actual_xml_changes
      $$ text "</changelog>"
    | Summary `elem` opts || Verbose `elem`  opts =
           vsep (map (number_patch change_with_summary) pis_and_fs)
        $$ errstring
    | otherwise = vsep (map (number_patch description') pis_and_fs)
               $$ errstring
    where change_with_summary :: (Sealed2 (PatchInfoAnd p), [FilePath]) -> Doc
          change_with_summary (Sealed2 hp, fs)
              | Just p <- hopefullyM hp = if showChangesOnlyToFiles opts
                                          then description hp $$ text "" $$
                                               indent (showFriendly opts (filterFLFL xx $ effect p))
                                          else showFriendly opts p
              | otherwise = description hp
                            $$ indent (text "[this patch is unavailable]")
              where xx x = case listTouchedFiles x of
                             ys | null $ ys `intersect` fs -> unsafeCoerce IsEq
                             -- in that case, the change does not affect the patches we are
                             -- looking at, so we ignore the difference between the two states.
                             -- It's all read-only anyway.
                             _ -> NotEq
          xml_with_summary (Sealed2 hp)
              | Just p <- hopefullyM hp = insertBeforeLastline
                                           (toXml $ info hp) (indent $ xmlSummary p)
          xml_with_summary (Sealed2 hp) = toXml (info hp)
          indent = prefix "    "
          actual_xml_changes = if Summary `elem` opts
                               then map xml_with_summary pis
                               else map (toXml . (unseal2 info)) pis
          xml_file_names = map (createdAsXml first_change) orig_fs
          first_change = if doReverse opts
                         then unseal2 info $ head pis
                         else unseal2 info $ last pis
          number_patch f x = if NumberPatches `elem` opts
                             then case get_number (fst x) of
                                  Just n -> text (show n++":") <+> f x
                                  Nothing -> f x
                             else f x
          get_number :: Sealed2 (PatchInfoAnd p) -> Maybe Int
          get_number (Sealed2 y) = gn 1 (newset2RL patchset)
              where iy = info y
                    gn :: Int -> RL (PatchInfoAnd p) C(start y) -> Maybe Int
                    gn n (b:<:bs) | seq n (info b) == iy = Just n
                                  | otherwise = gn (n+1) bs
                    gn _ NilRL = Nothing
          pis = map fst pis_and_fs
          description' = unseal2 description . fst

-- FIXME: this prose is unreadable. --twb, 2009-08
changesHelp'' :: String
changesHelp'' =
 "Three output formats exist.  The default is --human-readable.  You can\n" ++
 "also select --context, which is the internal format (as seen in patch\n" ++
 "bundles) that can be re-read by Darcs (e.g. `darcs get --context').\n" ++
 "\n" ++
 "Finally, there is --xml-output, which emits valid XML... unless a the\n" ++
 "patch metadata (author, name or description) contains a non-ASCII\n" ++
 "character and was recorded in a non-UTF8 locale.\n" ++
 "\n" ++
 -- FIXME: can't we just disallow the following usage?
 "Note that while the --context flag may be used in conjunction with\n" ++
 "--xml-output or --human-readable, in neither case will darcs get be\n" ++
 "able to read the output.  On the other hand, sufficient information\n" ++
 "WILL be output for a knowledgeable human to recreate the current state\n" ++
 "of the repository.\n"

changesContext :: [DarcsFlag] -> IO ()
changesContext opts = do
  let repodir = fromMaybe "." $ getRepourl opts
  withRepositoryDirectory opts repodir $ RepoJob $ \repository -> do
  (_ :> ps') <- contextPatches `fmap` readRepo repository
  let ps = mapRL (\p -> (seal2 p, [])) ps'
  unless fancy $ putStrLn "\nContext:\n"
  putDocLnWith simplePrinters $ changelog opts' emptyset (ps, [], empty)
    where opts' = if fancy then opts else MachineReadable : opts
          fancy = HumanReadable `elem` opts || XMLOutput `elem` opts
          emptyset = PatchSet NilRL NilRL

log :: DarcsCommand
log = commandAlias "log" Nothing changes
\end{code}
