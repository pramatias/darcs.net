%  Copyright (C) 2002-2005,2007-2008 David Roundy
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

\chapter{DarcsRepo format}

A repository consists of a working directory, which has within it a
directory called \verb!_darcs!. There must also be a subdirectory within
\verb!_darcs! named \verb!patches!.  The \verb!patches! directory contains
the actual patches which are in the repository.  There must also be a
\emph{pristine tree}, which may either be a directory containing a cache of
the version of the tree which has been recorded, or a stub, and may be
named either ``current'' or ``pristine''.

\emph{WARNING!} Viewing files in the pristine cache is perfectly
acceptable, but if you view them with an editor (e.g.\ vi or Emacs), that
editor may create temporary files in the pristine tree
(\verb|_darcs/pristine/| or \verb|_darcs/current/|), which will temporarily
cause your repository to be inconsistent.  So \emph{don't record any
patches while viewing files in \_darcs/current with an editor!}  A better
plan would be to restrict yourself to viewing these files with a pager such
as more or less.

Also within \verb!_darcs! is the \verb!inventory! file, which lists all the
patches that are in the repository. Moreover, it also gives the order of the
representation of the patches as they are stored. Given a source of patches,
i.e.\ any other set of repositories which have between them all the patches
contained in a given repository, that repository can be reproduced based on only the
information in the \verb!inventory! file. Under those circumstances, the
order of the patches specified in the \verb!inventory! file would be
unimportant, as this order is only needed to provide context for the
interpretation of the stored patches in this repository.

\begin{code}
{-# LANGUAGE CPP, ScopedTypeVariables #-}

#include "gadts.h"

module Darcs.Repository.DarcsRepo ( writeInventory, writeInventoryAndPatches,
                                    addToTentativePristine,
                                    addToTentativeInventory, removeFromTentativeInventory,
                                    finalizeTentativeChanges, finalizePristineChanges,
                                    revertTentativeChanges,
                                    readRepo, readTentativeRepo, writeAndReadPatch,
                                    copyPatches
                                  ) where

import System.Directory ( createDirectoryIfMissing )
import Workaround ( renameFile )
import Darcs.Utils ( clarifyErrors )
import Progress ( debugMessage, beginTedious, endTedious, finishedOneIO )
import Darcs.RepoPath ( ioAbsoluteOrRemote, toPath )
import System.IO ( hPutStrLn, stderr )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.FilePath.Posix ( (</>) )
import Control.Monad ( when )
import Darcs.Patch.PatchInfoAnd ( Hopefully, PatchInfoAnd,
                         patchInfoAndPatch, info,
                         actually, hopefully, unavailable, n2pia )
import Darcs.SignalHandler ( withSignalsBlocked )

import ByteStringUtils ( gzReadFilePS )
import qualified Data.ByteString as B ( readFile )
import qualified Data.ByteString.Char8 as BC (break, pack)

import Darcs.Patch ( RepoPatch, Effect, PrimOf, Named, invert,
                     effect,
                     patch2patchinfo,
                     readPatch,
                     showPatch )
import qualified Darcs.Patch as Patch
import Darcs.Patch.Prim.V1 ( Prim )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Show ( ShowPatchBasic )

import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), (+<+),
                             reverseFL, mapFL,
                             reverseRL, mapRL )
import Darcs.Patch.Info ( PatchInfo, makeFilename,
                          showPatchInfo, isTag, readPatchInfos
                 )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, newset2RL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.External ( gzFetchFilePS, copyFilesOrUrls, Cachable(..),
                        cloneFile )
import Darcs.Lock ( writeBinFile, writeDocBinFile, appendDocBinFile, appendBinFile )
import Darcs.Flags ( Compression(..), RemoteDarcs )
import Darcs.Patch.Depends ( slightlyOptimizePatchset, commuteToEnd, deepOptimizePatchset )
import Darcs.Repository.Pristine ( identifyPristine, applyPristine )
import Darcs.Global ( darcsdir )
import Darcs.ProgressPatches ( progressFL )
import Printer ( text, (<>), Doc, ($$), empty )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), seal, unseal, mapSeal )

#include "impossible.h"
\end{code}

There is a very special patch which may be stored in \verb!patches! which
is called `pending'.  This patch describes any changes which have not yet
been recorded, and cannot be determined by a simple diff.  For example, file
additions or renames are placed in pending until they are recorded.
Similarly, token replaces are stored in pending until they are recorded.

\begin{code}
writePatch :: RepoPatch p => Compression -> Named p C(x y) -> IO FilePath
writePatch compr p =
       do let writeFun = case compr of
                NoCompression -> Patch.writePatch
                GzipCompression -> Patch.gzWritePatch
              pname = darcsdir++"/patches/"++makeFilename (patch2patchinfo p)
          writeFun pname p
          return pname

writeAndReadPatch :: RepoPatch p => Compression -> PatchInfoAnd p C(x y)
                     -> IO (PatchInfoAnd p C(x y))
writeAndReadPatch compr p =
    do fn <- writePatch compr $ hopefully p
       unsafeInterleaveIO $ parse fn
    where parse fn = do debugMessage ("Reading patch file: "++ fn)
                        ps <- gzReadFilePS fn
                        Sealed pp <- case readPatch ps of
                                    Just x -> return x
                                    Nothing -> fail ("Couldn't parse patch file "++fn)
                        return $ n2pia $ unsafeCoerceP pp

--formatInventory is not exported for use outside of the DarcsRepo module
--itself.
formatInventory :: [PatchInfo] -> Doc
formatInventory [] = empty
formatInventory (pinfo:ps) = showPatchInfo pinfo $$ formatInventory ps

writeInventory :: RepoPatch p => FilePath -> PatchSet p C(Origin x) -> IO ()
-- Note that writeInventory optimizes the inventory it writes out by
-- checking on tag dependencies.
-- FIXME: There is also a problem that writeInventory always writes
-- out the entire inventory, including the parts that you haven't
-- changed...
writeInventory dir ps = withSignalsBlocked $ do
    createDirectoryIfMissing False (dir++"/"++darcsdir++"/inventories")
    simplyWriteInventory "inventory" dir $ slightlyOptimizePatchset ps

simplyWriteInventory :: RepoPatch p => String -> FilePath -> PatchSet p C(Origin x) -> IO ()
simplyWriteInventory name dir (PatchSet NilRL NilRL) =
    writeBinFile (dir++"/"++darcsdir++"/"++name) ""
simplyWriteInventory name dir (PatchSet ps NilRL) = do
    writeDocBinFile (dir++"/"++darcsdir++"/"++name) $ formatInventory $ mapFL info $ reverseRL ps
simplyWriteInventory name dir (PatchSet ps ts@(Tagged t _ _ :<: _)) = do -- nonempty Tagged
    simplyWriteTaggedInventory dir ts
    writeDocBinFile (dir++"/"++darcsdir++"/"++name) $ text "Starting with tag:"
                                           $$ formatInventory (mapFL info $ t :>: reverseRL ps)

simplyWriteTaggedInventory :: RepoPatch p => FilePath -> RL (Tagged p) C(Origin x) -> IO ()
simplyWriteTaggedInventory _ NilRL = return ()
simplyWriteTaggedInventory dir (Tagged t _ ps :<: NilRL) = do
    writeDocBinFile (dir </> "_darcs/inventories" </> makeFilename (info t)) $
                    formatInventory (mapFL info $ reverseRL ps)
simplyWriteTaggedInventory dir (Tagged t _ ps :<: ts@(Tagged t2 _ _ :<: _)) =
    do simplyWriteTaggedInventory dir ts
       writeDocBinFile (dir </> "_darcs/inventories" </> makeFilename (info t)) $
                       text "Starting with tag:" $$
                       formatInventory (mapFL info $ t2 :>: reverseRL ps)

writeInventoryAndPatches :: RepoPatch p => Compression -> PatchSet p C(Origin x) -> IO ()
writeInventoryAndPatches compr ps =   do writeInventory "." ps
                                         sequence_ $ mapRL (writePatch compr . hopefully) $ newset2RL ps

addToTentativeInventory :: forall p C(x y). RepoPatch p => Compression -> Named p C(x y) -> IO FilePath
addToTentativeInventory compr p =
    do appendDocBinFile (darcsdir++"/tentative_inventory") $ text "\n"
                            <> showPatchInfo (patch2patchinfo p)
       res <- writePatch compr p
       when (isTag $ patch2patchinfo p) $
            do debugMessage "Optimizing the tentative inventory, since we're adding a tag."
               realdir <- toPath `fmap` ioAbsoluteOrRemote "."
               let k = "Reading tentative inventory"
               beginTedious k
               Sealed ps <- readRepoPrivate k realdir "tentative_inventory"
                            :: IO  (SealedPatchSet p C(Origin) )
               simplyWriteInventory "tentative_inventory" "." $ slightlyOptimizePatchset ps
       return res

addToTentativePristine :: (Effect p, PatchListFormat (PrimOf p), ShowPatchBasic (PrimOf p)) => p C(x y) -> IO ()
addToTentativePristine p =
    do -- Sealed p <- (fst . fromJust . readPatchCarefully) `fmap` gzReadFilePS fp
       appendDocBinFile (darcsdir++"/tentative_pristine") $ showPatch (effect p) -- FIXME: this is inefficient!
       appendBinFile (darcsdir++"/tentative_pristine") "\n"

removeFromTentativeInventory :: RepoPatch p => Bool -> Compression
                                -> FL (PatchInfoAnd p) C(x y) -> IO ()
removeFromTentativeInventory update_pristine compr to_remove =
    do finalizeTentativeChanges
       Sealed allpatches <- readRepo "."
       unmodified :> skipped <- return $ commuteToEnd
                                  (reverseFL $ unsafeCoerceP to_remove) allpatches
       sequence_ $ mapRL (writePatch compr . hopefully) skipped
       let newpatches = case unmodified of
                        PatchSet ps ts -> PatchSet (skipped+<+ps) ts
       writeInventory "." $ deepOptimizePatchset newpatches
       when update_pristine $
            do pris <- identifyPristine
               repairable $ applyPristine pris
                              $ progressFL "Applying inverse to pristine" $ invert to_remove
       revertTentativeChanges

finalizeTentativeChanges :: IO ()
finalizeTentativeChanges = renameFile (darcsdir++"/tentative_inventory") (darcsdir++"/inventory")

finalizePristineChanges :: IO ()
finalizePristineChanges =
    do Sealed ps <- read_patches $ darcsdir++"/tentative_pristine"
       pris <- identifyPristine
       repairable $ applyPristine pris ps
    where
      read_patches :: String -> IO (Sealed (FL Prim C(x)))
      read_patches f = do ps <- B.readFile f
                          return $ case readPatch ps of
                                   Just x -> x
                                   Nothing -> seal $ NilFL

repairable :: IO a -> IO a
repairable x = x `clarifyErrors` unlines
               ["Your repository is now in an inconsistent state.",
                "This must be fixed by running darcs repair."]

revertTentativeChanges :: IO ()
revertTentativeChanges =
    do cloneFile (darcsdir++"/inventory") (darcsdir++"/tentative_inventory")
       writeBinFile (darcsdir++"/tentative_pristine") ""

copyPatches :: RemoteDarcs -> FilePath -> FilePath -> [PatchInfo] -> IO ()
copyPatches remote dir out patches = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote dir
  copyFilesOrUrls remote (realdir++"/"++darcsdir++"/patches") (map makeFilename patches)
                         (out++"/"++darcsdir++"/patches") Cachable

readRepo :: RepoPatch p => String -> IO (SealedPatchSet p C(Origin))
readRepo d = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote d
  let k = "Reading inventory of repository "++d
  beginTedious k
  readRepoPrivate k realdir "inventory" `catch`
                        (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                                  ioError e)

readTentativeRepo :: RepoPatch p => String -> IO (SealedPatchSet p C(Origin))
readTentativeRepo d = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote d
  let k = "Reading tentative inventory of repository "++d
  beginTedious k
  readRepoPrivate k realdir "tentative_inventory" `catch`
                        (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                                  ioError e)

readRepoPrivate :: RepoPatch p => String -> FilePath -> FilePath -> IO (SealedPatchSet p C(Origin))
readRepoPrivate k d iname = do
    i <- gzFetchFilePS (d </> "_darcs" </> iname) Uncachable
    finishedOneIO k iname
    let parse inf = parse2 inf $ d </> "_darcs/patches" </> makeFilename inf
        (mt, is) = case BC.break ((==) '\n') i of
                   (swt,pistr) | swt == BC.pack "Starting with tag:" ->
                                     case readPatchInfos pistr of
                                     (t:ids) -> (Just t,reverse ids)
                                     [] -> bug "bad inventory in readRepoPrivate"
                   _ -> (Nothing, reverse $ readPatchInfos i)
    Sealed ts <- unseal seal `fmap` unsafeInterleaveIO (read_ts parse mt)
    Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
    return $ seal (PatchSet ps ts)
    where read_ts :: RepoPatch p =>
                     (FORALL(b) PatchInfo -> IO (Sealed (PatchInfoAnd p C(b))))
                  -> Maybe PatchInfo -> IO (Sealed (RL (Tagged p) C(Origin)))
          read_ts _ Nothing = do endTedious k
                                 return $ seal NilRL
          read_ts parse (Just tag0) =
              do debugMessage $ "Looking for inventory for:\n"++ show tag0
                 i <- unsafeInterleaveIO $
                      do x <- gzFetchFilePS (d</>"_darcs/inventories"</>makeFilename tag0) Uncachable
                         finishedOneIO k (show tag0)
                         return x
                 let (mt, is) = case BC.break ((==) '\n') i of
                                (swt,pistr) | swt == BC.pack "Starting with tag:" ->
                                                case readPatchInfos pistr of
                                                (t:ids) -> (Just t,reverse ids)
                                                [] -> bug "bad inventory in readRepoPrivate"
                                _ -> (Nothing, reverse $ readPatchInfos i)
                 Sealed ts <- fmap (unseal seal) $ unsafeInterleaveIO $ read_ts parse mt
                 Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches parse is)
                 Sealed tag00 <-  parse tag0 `catch`
                                  \e -> return $ seal $
                                        patchInfoAndPatch tag0 $ unavailable $ show e
                 return $ seal $ Tagged tag00 Nothing ps :<: ts
          parse2 :: RepoPatch p => PatchInfo -> FilePath
                                -> IO (Sealed (PatchInfoAnd p C(x)))
          parse2 i fn = do ps <- unsafeInterleaveIO $ gzFetchFilePS fn Cachable
                           return $ patchInfoAndPatch i
                             `mapSeal` hopefullyNoParseError (toPath fn) (readPatch ps)
          hopefullyNoParseError :: String -> Maybe (Sealed (Named a1dr C(x)))
                                -> Sealed (Hopefully (Named a1dr) C(x))
          hopefullyNoParseError _ (Just (Sealed x)) = seal $ actually x
          hopefullyNoParseError s Nothing = seal $ unavailable $ "Couldn't parse file "++s
          read_patches :: RepoPatch p =>
                          (FORALL(b) PatchInfo -> IO (Sealed (PatchInfoAnd p C(b))))
                       -> [PatchInfo] -> IO (Sealed (RL (PatchInfoAnd p) C(x)))
          read_patches _ [] = return $ seal NilRL
          read_patches parse (i:is) =
              lift2Sealed (:<:)
                          (read_patches parse is)
                          (parse i `catch` \e ->
                           return $ seal $ patchInfoAndPatch i $ unavailable $ show e)
          lift2Sealed :: (FORALL(y z) q C(y z) -> pp C(y) -> r C(z))
                      -> IO (Sealed pp) -> (FORALL(b) IO (Sealed (q C(b)))) -> IO (Sealed r)
          lift2Sealed f iox ioy = do Sealed x <- unseal seal `fmap` unsafeInterleaveIO iox
                                     Sealed y <- unseal seal `fmap` unsafeInterleaveIO ioy
                                     return $ seal $ f y x

\end{code}

The \verb!_darcs! directory also contains a directory called
``\verb!prefs!'', which is described in Chapter~\ref{configuring}.
