%  Copyright (C) 2002-2005,2007 David Roundy
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

\darcsCommand{convert}
\begin{code}
{-# LANGUAGE CPP, MagicHash #-}

#include "gadts.h"

module Darcs.Commands.Convert ( convert ) where

import System.Directory ( setCurrentDirectory, doesDirectoryExist, doesFileExist,
                   createDirectory )
import Workaround ( getCurrentDirectory )
import Control.Monad ( when )
import GHC.Base ( unsafeCoerce# )
import Data.Maybe ( catMaybes )
import qualified Data.ByteString as B

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, n2pia, info, hopefully )
import Darcs.Commands ( DarcsCommand(..), nodefaults, putInfo )
import Darcs.Arguments
   ( DarcsFlag
      ( AllowConflicts, NewRepo, SetScriptsExecutable, UseFormat2
      , NoUpdateWorking
      )
   , reponame
   , setScriptsExecutableOption
   , networkOptions
   )
import Darcs.Repository ( Repository, withRepoLock, RepoJob(..), withRepositoryDirectory, readRepo,
                          createRepository, invalidateIndex,
                          optimizeInventory,
                          tentativelyMergePatches, patchSetToPatches,
                          createPristineDirectoryTree,
                          revertRepositoryChanges, finalizeRepositoryChanges,
                          applyToWorking, setScriptsExecutable )
import Darcs.Global ( darcsdir )
import Darcs.Patch ( Named, showPatch, patch2patchinfo, fromPrim, fromPrims,
                     infopatch, adddeps, getdeps, effect, patchcontents )
import Darcs.Witnesses.Eq ( EqCheck(..), (=/\=) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), bunchFL, mapFL, mapFL_FL,
                                 concatFL, mapRL )
import Darcs.Patch.Info ( piRename, piTag, isTag, PatchInfo )
import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.V1.Commute ( publicUnravel )
import Darcs.Patch.V1.Core ( Patch(PP), isMerger )
import Darcs.Patch.V2.Real ( mergeUnravelled )
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), newset2RL )
import Darcs.RepoPath ( ioAbsoluteOrRemote, toPath )
import Darcs.Repository.Format(identifyRepoFormat, formatHas, RepoProperty(Darcs2))
import Darcs.Repository.Motd ( showMotd )
import Darcs.Utils ( clarifyErrors, askUser, catchall )
import Darcs.ProgressPatches ( progressFL )
import Darcs.Witnesses.Sealed ( FlippedSeal(..), Sealed(..) )
import Printer ( text, ($$) )
import Darcs.ColorPrinter ( traceDoc )
import Darcs.Lock ( writeBinFile )
import Darcs.External ( fetchFilePS, Cachable(Uncachable) )
import System.FilePath.Posix

#include "gadts.h"
#include "impossible.h"

convertDescription :: String
convertDescription = "Convert a repository from a legacy format."

convertHelp :: String
convertHelp =
 "The current repository format is called `darcs-2'.  It was introduced\n" ++
 "in Darcs 2.0 and became the default for new projects in Darcs 2.2.\n" ++
 "The `darcs convert' command allows existing projects to migrate to\n" ++
 "this format from the older `darcs-1' format.\n" ++
 "\n" ++
 "This command DOES NOT modify the source repository; a new destination\n" ++
 "repository is created.  It is safe to run this command more than once\n" ++
 "on a repository (e.g. for testing), before the final conversion.\n" ++
 "\n" ++
 convertHelp' ++
 "\n" ++
 "Due to this limitation, migrating a multi-branch project is a little\n" ++
 "awkward.  Sorry!  Here is the recommended process:\n" ++
 "\n" ++
 " 1. for each branch `foo', tag that branch with `foo-final';\n" ++
 " 2. merge all branches together (--allow-conflicts may help);\n" ++
 " 3. run `darcs optimize --reorder' on the result;\n" ++
 " 4. run `darcs convert' to create a merged darcs-2 repository;\n" ++
 " 5. re-create each branch by calling `darcs get --tag foo-final' on\n" ++
 "    the darcs-2 repository; and finally\n" ++
 " 6. use `darcs obliterate' to delete the foo-final tags.\n"

-- | This part of the help is split out because it is used twice: in
-- the help string, and in the prompt for confirmation.
convertHelp' :: String
convertHelp' =
 "WARNING: the repository produced by this command is not understood by\n" ++
 "Darcs 1.x, and patches cannot be exchanged between repositories in\n" ++
 "darcs-1 and darcs-2 formats.\n" ++
 "\n" ++
 "Furthermore, darcs 2 repositories created by different invocations of\n" ++
 "this command SHOULD NOT exchange patches, unless those repositories\n" ++
 "had no patches in common when they were converted.  (That is, within a\n" ++
 "set of repos that exchange patches, no patch should be converted more\n" ++
 "than once.)\n"

convert :: DarcsCommand
convert = DarcsCommand {commandProgramName = "darcs",
                    commandName = "convert",
                    commandHelp = convertHelp,
                    commandDescription = convertDescription,
                    commandExtraArgs = -1,
                    commandExtraArgHelp = ["<SOURCE>", "[<DESTINATION>]"],
                    commandCommand = convertCmd,
                    commandPrereq = \_ -> return $ Right (),
                    commandGetArgPossibilities = return [],
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = networkOptions,
                    commandBasicOptions = [reponame,setScriptsExecutableOption]}

convertCmd :: [DarcsFlag] -> [String] -> IO ()
convertCmd opts [inrepodir, outname] = convertCmd (NewRepo outname:opts) [inrepodir]
convertCmd orig_opts [inrepodir] = do

  typed_repodir <- ioAbsoluteOrRemote inrepodir
  let repodir = toPath typed_repodir

  --test for converting darcs-2 repository
  format <- either fail return =<< identifyRepoFormat repodir
  when (formatHas Darcs2 format) $ fail "Repository is already in darcs 2 format."

  putStrLn convertHelp'
  let vow = "I understand the consequences of my action"
  putStrLn "Please confirm that you have read and understood the above"
  vow' <- askUser ("by typing `" ++ vow ++ "': ")
  when (vow' /= vow) $ fail "User didn't understand the consequences."

  let opts = UseFormat2:orig_opts
  showMotd opts repodir
  mysimplename <- makeRepoName opts repodir
  createDirectory mysimplename
  setCurrentDirectory mysimplename
  createRepository opts
  writeBinFile (darcsdir++"/hashed_inventory") ""
  withRepoLock (NoUpdateWorking:opts) $ V2Job $ \repository ->
    withRepositoryDirectory opts repodir $ V1Job $ \themrepo -> do
      theirstuff <- readRepo themrepo
      let patches = mapFL_FL convertNamed $ patchSetToPatches theirstuff
          inOrderTags = iot theirstuff
              where iot :: PatchSet p C(s x) -> [PatchInfo]
                    iot (PatchSet _ ts) = iot_ ts
                    iot_ :: RL(Tagged t1) C(t y) -> [PatchInfo]
                    iot_ (Tagged t _ _ :<: ts) = info t : iot_ ts
                    iot_ NilRL = []
          outOfOrderTags = catMaybes $ mapRL oot $ newset2RL theirstuff
              where oot t = if isTag (info t) && not (info t `elem` inOrderTags)
                            then Just (info t, getdeps $ hopefully t)
                            else Nothing
          fixDep p = case lookup p outOfOrderTags of
                     Just d -> p : concatMap fixDep d
                     Nothing -> [p]
          convertOne :: Patch Prim C(x y) -> FL (RealPatch Prim) C(x y)
          convertOne x | isMerger x = case mergeUnravelled $ publicUnravel x of
                                       Just (FlippedSeal y) ->
                                           case effect y =/\= effect x of
                                           IsEq -> y :>: NilFL
                                           NotEq ->
                                               traceDoc (text "lossy conversion:" $$
                                                         showPatch x)
                                               fromPrims (effect x)
                                       Nothing -> traceDoc (text
                                                            "lossy conversion of complicated conflict:" $$
                                                            showPatch x)
                                                  fromPrims (effect x)
          convertOne (PP x) = fromPrim x :>: NilFL
          convertOne _ = impossible
          convertFL :: FL (Patch Prim) C(x y) -> FL (RealPatch Prim) C(x y)
          convertFL = concatFL . mapFL_FL convertOne
          convertNamed :: Named (Patch Prim) C(x y) -> PatchInfoAnd (RealPatch Prim) C(x y)
          convertNamed n = n2pia $
                           adddeps (infopatch (convertInfo $ patch2patchinfo n) $
                                              convertFL $ patchcontents n)
                                   (map convertInfo $ concatMap fixDep $ getdeps n)
          convertInfo n | n `elem` inOrderTags = n
                        | otherwise = maybe n (\t -> piRename n ("old tag: "++t)) $ piTag n
          applySome xs = do -- TODO this unsafeCoerce hack is because we don't keep track of the repository state properly
                            -- Really sequence_ $ mapFL applySome below should instead be a repeated add operation -
                            -- there doesn't seem to be any reason we need to do a merge here.
                            let repository2 = unsafeCoerce# repository :: Repository (RealPatch Prim) C(a b a)
                            Sealed pw <- tentativelyMergePatches repository2 "convert" (AllowConflicts:opts) NilFL xs
                            finalizeRepositoryChanges repository2 -- this is to clean out pristine.hashed
                            revertRepositoryChanges repository2
                            _ <- revertable $ applyToWorking repository2 opts pw
                            invalidateIndex repository2
      sequence_ $ mapFL applySome $ bunchFL 100 $ progressFL "Converting patch" patches
      invalidateIndex repository
      revertable $ createPristineDirectoryTree repository "."
      when (SetScriptsExecutable `elem` opts) $ setScriptsExecutable

      -- Copy over the prefs file
      let prefsRelPath = darcsdir </> "prefs" </> "prefs"
      (fetchFilePS (repodir </> prefsRelPath) Uncachable >>= B.writeFile prefsRelPath)
       `catchall` return ()

      optimizeInventory repository
      putInfo opts $ text "Finished converting."
      where revertable x = x `clarifyErrors` unlines
                  ["An error may have left your new working directory an inconsistent",
                   "but recoverable state. You should be able to make the new",
                   "repository consistent again by running darcs revert -a."]

convertCmd _ _ = fail "You must provide 'convert' with either one or two arguments."

makeRepoName :: [DarcsFlag] -> FilePath -> IO String
makeRepoName (NewRepo n:_) _ =
    do exists <- doesDirectoryExist n
       file_exists <- doesFileExist n
       if exists || file_exists
          then fail $ "Directory or file named '" ++ n ++ "' already exists."
          else return n
makeRepoName (_:as) d = makeRepoName as d
makeRepoName [] d =
  case dropWhile (=='.') $ reverse $
       takeWhile (\c -> c /= '/' && c /= ':') $
       dropWhile (=='/') $ reverse d of
  "" -> modifyRepoName "anonymous_repo"
  base -> modifyRepoName base

modifyRepoName :: String -> IO String
modifyRepoName name =
    if head name == '/'
    then mrn name (-1)
    else do cwd <- getCurrentDirectory
            mrn (cwd ++ "/" ++ name) (-1)
 where
  mrn :: String -> Int -> IO String
  mrn n i = do
    exists <- doesDirectoryExist thename
    file_exists <- doesFileExist thename
    if not exists && not file_exists
       then do when (i /= -1) $
                    putStrLn $ "Directory '"++ n ++
                               "' already exists, creating repository as '"++
                               thename ++"'"
               return thename
       else mrn n $ i+1
    where thename = if i == -1 then n else n++"_"++show i

\end{code}
