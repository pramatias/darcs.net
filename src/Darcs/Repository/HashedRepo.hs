-- Copyright (C) 2006-2007 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software Foundation,
-- Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

{-# LANGUAGE CPP, ScopedTypeVariables #-}

#include "gadts.h"

module Darcs.Repository.HashedRepo ( revertTentativeChanges, finalizeTentativeChanges,
                                     cleanPristine,
                                     copyPristine, copyPartialsPristine,
                                     applyToTentativePristine,
                                     addToTentativeInventory, removeFromTentativeInventory,
                                     readRepo, readTentativeRepo, writeAndReadPatch,
                                     writeTentativeInventory, copyRepo,
                                     readHashedPristineRoot, pris2inv, copySources,
                                     listInventories
                                   ) where

import System.Directory ( createDirectoryIfMissing )
import System.FilePath.Posix( (</>) )
import System.IO.Unsafe ( unsafeInterleaveIO )
import System.IO ( stderr, hPutStrLn )
import Data.List ( delete )
import Control.Monad ( unless )
import Control.Applicative ( (<$>) )

import Workaround ( renameFile )
import Darcs.Flags ( Compression, RemoteDarcs )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.RepoPath ( FilePathLike, ioAbsoluteOrRemote, toPath )
import Darcs.Repository.Cache ( Cache(..), CacheLoc(..), fetchFileUsingCache,
                                speculateFilesUsingCache, writeFileUsingCache,
                                unionCaches, repo2cache, okayHash, takeHash,
                                HashedDir(..), hashedDir,
                                peekInCache )
import qualified Darcs.Repository.Cache as DarcsCache
import Darcs.Repository.HashedIO ( copyHashed, copyPartialsHashed,
                                   cleanHashdir )
import Darcs.Repository.InternalTypes ( Repository(..), extractCache, modifyCache )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, patchInfoAndPatch, info,
                         extractHash, createHashed )
import Darcs.Patch ( RepoPatch, Patchy, showPatch, readPatch, apply )
import Darcs.Patch.Patchy ( Apply )
import Darcs.Patch.ReadMonads ( parseStrictly )
import Darcs.Patch.Depends ( commuteToEnd, slightlyOptimizePatchset )
import Darcs.Patch.Info ( PatchInfo, showPatchInfo, humanFriendly, readPatchInfo )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( reverseRL, reverseFL, (+<+) )

import ByteStringUtils ( gzReadFilePS, dropSpace )
import qualified Data.ByteString as B (null, length, empty
                                      ,tail, take, drop, ByteString)
import qualified Data.ByteString.Char8 as BC (unpack, dropWhile, break, pack)

import Printer ( Doc, hcat, (<>), ($$), renderString, renderPS, text, invisiblePS )
import Darcs.ColorPrinter () -- for instance Show Doc
import Crypt.SHA256 ( sha256sum )
import Darcs.External ( copyFileOrUrl, cloneFile, fetchFilePS, gzFetchFilePS,
    Cachable( Uncachable ) )
import Darcs.Lock ( writeBinFile, writeDocBinFile, writeAtomicFilePS, appendBinFile, appendDocBinFile )
import Darcs.Utils ( withCurrentDirectory )
import Progress ( beginTedious, endTedious, debugMessage, finishedOneIO )
#include "impossible.h"
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (:>)(..), mapRL, mapFL )
import Darcs.Witnesses.Sealed ( Sealed(..), seal, unseal, mapSeal )
import Darcs.Global ( darcsdir )

import Storage.Hashed.Darcs( hashedTreeIO, readDarcsHashedNosize, readDarcsHashed,
                             writeDarcsHashed,
                             decodeDarcsHash, decodeDarcsSize )
import Storage.Hashed.Tree( treeHash )
import Storage.Hashed.Hash( encodeBase16, Hash(..) )

applyHashed'
   :: Apply p => Hash -> p C(x y) -> IO String
applyHashed' root p = do case root of
                              (SHA256 _) -> return ()
                              _ -> fail $ "Cannot handle hash: " ++ show root
                         s <- readDarcsHashedNosize "_darcs/pristine.hashed" root
                         (_, t) <- (hashedTreeIO (apply p) s "_darcs/pristine.hashed")
                         return $ BC.unpack . encodeBase16 $ treeHash t

applyHashed :: Patchy q => String -> q C(x y) -> IO String
applyHashed h p = applyHashed' hash p `catch` \_ -> do
                          hPutStrLn stderr warn
                          inv <- gzReadFilePS invpath
                          let oldroot = BC.pack $ inv2pris inv
                              oldroot_hash = decodeDarcsHash oldroot
                              oldroot_size = decodeDarcsSize oldroot
                          old <- readDarcsHashed "_darcs/pristine.hashed" (oldroot_size, oldroot_hash)
                          root <- writeDarcsHashed old "_darcs/pristine.hashed"
                          let newroot = BC.unpack $ encodeBase16 root
                          writeDocBinFile invpath $ pris2inv newroot inv
                          cleanHashdir (Ca []) HashedPristineDir [newroot]
                          hPutStrLn stderr "Pristine conversion done..."
                          applyHashed' root p
  where invpath = darcsdir ++ "/hashed_inventory"
        hash = decodeDarcsHash $ BC.pack h
        warn = "WARNING: Doing a one-time conversion of pristine format.\n" ++
               "This may take a while. The new format is backwards-compatible."

revertTentativeChanges :: IO ()
revertTentativeChanges =
    do cloneFile (darcsdir++"/hashed_inventory") (darcsdir++"/tentative_hashed_inventory")
       i <- gzReadFilePS (darcsdir++"/hashed_inventory")
       writeBinFile (darcsdir++"/tentative_pristine") $ "pristine:" ++ inv2pris i

finalizeTentativeChanges :: RepoPatch p => Repository p C(r u t) -> Compression -> IO ()
finalizeTentativeChanges r compr =
    do let t = darcsdir++"/tentative_hashed_inventory"
       -- first let's optimize it...
       debugMessage "Optimizing the inventory..."
       ps <- readTentativeRepo r "."
       writeTentativeInventory (extractCache r) compr ps
       -- then we'll add in the pristine cache,
       i <- gzReadFilePS t
       p <- gzReadFilePS $ darcsdir++"/tentative_pristine"
       writeDocBinFile t $ pris2inv (inv2pris p) i
       -- and rename it to its final value
       renameFile t $ darcsdir++"/hashed_inventory"
       -- note: in general we can't clean the pristine cache, because a
       -- simultaneous get might be in progress

readHashedPristineRoot :: Repository p C(r u t) -> IO (Maybe String)
readHashedPristineRoot (Repo d _ _ _) =
    withCurrentDirectory d $ do
      i <- (Just `fmap` gzReadFilePS (darcsdir++"/hashed_inventory")) `catch` (\_ -> return Nothing)
      return $ inv2pris `fmap` i

cleanPristine :: Repository p C(r u t) -> IO ()
cleanPristine r@(Repo d _ _ _) = withCurrentDirectory d $
   do -- we'll remove obsolete bits of our pristine cache
      debugMessage "Cleaning out the pristine cache..."
      i <- gzReadFilePS (darcsdir++"/hashed_inventory")
      cleanHashdir (extractCache r) HashedPristineDir [inv2pris i]

addToTentativeInventory :: RepoPatch p => Cache -> Compression
                           -> PatchInfoAnd p C(x y) -> IO FilePath
addToTentativeInventory c compr p =
    do hash <- snd `fmap` writePatchIfNecesary c compr p
       appendDocBinFile (darcsdir++"/tentative_hashed_inventory") $ showPatchInfo $ info p
       appendBinFile (darcsdir++"/tentative_hashed_inventory") $ "\nhash: " ++ hash ++ "\n"
       return $ darcsdir++"/patches/" ++ hash

removeFromTentativeInventory :: RepoPatch p => Repository p C(r u t) -> Compression
                                -> FL (PatchInfoAnd p) C(x t) -> IO ()
removeFromTentativeInventory repo compr to_remove =
       -- FIXME: This algorithm should be *far* simpler.  All we need do is
       -- to to remove the patches from a patchset and then write that
       -- patchset.  The commutation behavior of PatchInfoAnd should track
       -- which patches need to be rewritten for us.
    do allpatches <- readTentativeRepo repo "."
       _ :> skipped <- return $ commuteToEnd (reverseFL to_remove) allpatches
       okay <- simpleRemoveFromTentativeInventory repo compr
               (mapFL info to_remove ++ mapRL info skipped)
       unless okay $ bug "bug in HashedRepo.removeFromTentativeInventory"
       sequence_ $ mapFL (addToTentativeInventory (extractCache repo) compr) (reverseRL skipped)

simpleRemoveFromTentativeInventory :: forall p C(r u t). RepoPatch p =>
                                          Repository p C(r u t) -> Compression -> [PatchInfo] -> IO Bool
simpleRemoveFromTentativeInventory repo compr pis = do
    inv <- readTentativeRepo repo "."
    case cut_inv pis inv of
      Nothing -> return False
      Just (Sealed inv') -> do writeTentativeInventory (extractCache repo) compr inv'
                               return True
    where cut_inv :: [PatchInfo] -> PatchSet p C(start x) -> Maybe (SealedPatchSet p C(start))
          cut_inv [] x = Just $ seal x
          cut_inv x (PatchSet NilRL (Tagged t _ ps :<: ts))
              = cut_inv x (PatchSet (t :<: ps) ts)
          cut_inv xs (PatchSet (hp:<:r) ts)
              | info hp `elem` xs = cut_inv (info hp `delete` xs) (PatchSet r ts)
          cut_inv _ _ = Nothing

writeHashFile :: Cache -> Compression -> HashedDir -> Doc -> IO String
writeHashFile c compr subdir d = do debugMessage $ "Writing hash file to "++(hashedDir subdir)
                                    writeFileUsingCache c compr subdir $ renderPS d

readRepo :: RepoPatch p => Repository p C(r u t) -> String -> IO (PatchSet p C(Origin r))
readRepo repo d = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote d
  Sealed ps <- readRepoPrivate (extractCache repo) realdir "hashed_inventory" `catch`
                 (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                           ioError e)
  return $ unsafeCoerceP ps

readTentativeRepo :: RepoPatch p => Repository p C(r u t) -> String -> IO (PatchSet p C(Origin t))
readTentativeRepo repo d = do
  realdir <- toPath `fmap` ioAbsoluteOrRemote d
  Sealed ps <- readRepoPrivate (extractCache repo) realdir "tentative_hashed_inventory" `catch`
                 (\e -> do hPutStrLn stderr ("Invalid repository:  " ++ realdir)
                           ioError e)
  return $ unsafeCoerceP ps

readRepoPrivate :: RepoPatch p => Cache -> FilePath -> FilePath -> IO (SealedPatchSet p C(Origin))
readRepoPrivate cache d iname =
 do inventory <- readInventoryPrivate cache (d </> "_darcs") iname
    parseinvs inventory
    where read_patches :: RepoPatch p => [(PatchInfo, String)]
                       -> IO (Sealed (RL (PatchInfoAnd p) C(x)))
          read_patches [] = return $ seal NilRL
          read_patches allis@((i1,h1):is1) =
              lift2Sealed (\p rest -> i1 `patchInfoAndPatch` p :<: rest)
                          (rp is1)
                          (createHashed h1 (const $ speculate h1 allis >> parse i1 h1))
              where rp :: RepoPatch p => [(PatchInfo, String)]
                       -> IO (Sealed (RL (PatchInfoAnd p) C(x)))
                    rp [] = return $ seal NilRL
                    rp [(i,h),(il,hl)] =
                        lift2Sealed (\p rest -> i `patchInfoAndPatch` p :<: rest)
                                    (rp [(il,hl)])
                                    (createHashed h (const $ speculate h (reverse allis) >> parse i h))
                    rp ((i,h):is) = lift2Sealed (\p rest -> i `patchInfoAndPatch` p :<: rest)
                                                (rp is)
                                                (createHashed h (parse i))
          read_tag :: RepoPatch p => (PatchInfo, String) -> IO (Sealed (PatchInfoAnd p C(x)))
          read_tag (i,h) = mapSeal (patchInfoAndPatch i) `fmap` createHashed h (parse i)
          speculate :: String -> [(PatchInfo, String)] -> IO ()
          speculate h is =
              do already_got_one <- peekInCache cache HashedPatchesDir h
                 unless already_got_one $
                        speculateFilesUsingCache cache HashedPatchesDir (map snd is)
          parse :: Patchy p => PatchInfo -> String -> IO (Sealed (p C(x)))
          parse i h = do debugMessage ("Reading patch file: "++ show (humanFriendly i))
                         (fn,ps) <- fetchFileUsingCache cache HashedPatchesDir h
                         case readPatch ps of
                           Just p -> return p
                           Nothing -> fail $ unlines ["Couldn't parse file "++fn,
                                                      "which is patch",
                                                      renderString $ humanFriendly i]
          parseinvs :: RepoPatch p => (Maybe String, [(PatchInfo, String)])
                    -> IO (SealedPatchSet p C(Origin))
          parseinvs (Nothing, ris) = mapSeal (\ps -> PatchSet ps NilRL)
                                     `fmap` (read_patches $ reverse ris)
          parseinvs (Just h, []) = bug $ "bad inventory "++h++" (no tag) in parseinvs!"
          parseinvs (Just h, t:ris) = do Sealed ts <- unseal seal `fmap`
                                                      unsafeInterleaveIO (read_ts t h)
                                         Sealed ps <- unseal seal `fmap`
                                             unsafeInterleaveIO (read_patches $
                                                                 reverse ris)
                                         return $ seal $ PatchSet ps ts
          read_ts :: RepoPatch p => (PatchInfo, String) -> String -> IO (Sealed (RL (Tagged p) C(Origin)))
          read_ts tag0 h0 =
              do contents <- unsafeInterleaveIO $ readTaggedInventory cache h0
                 let is = reverse $ case contents of (Just _, _:ris0) -> ris0
                                                     (Nothing, ris0) -> ris0
                                                     (Just _, []) -> bug "inventory without tag!!!!"
                 Sealed ts <- fmap (unseal seal) $ unsafeInterleaveIO $
                              case contents of
                              (Just h', t':_) -> read_ts t' h'
                              (Just _, []) -> bug "inventory without tag!!!!"
                              (Nothing, _) -> return $ seal NilRL
                 Sealed ps <- unseal seal `fmap` unsafeInterleaveIO (read_patches is)
                 Sealed tag00 <- read_tag tag0
                 return $ seal $ Tagged tag00 (Just h0) ps :<: ts
          lift2Sealed :: (FORALL(y z) q C(y z) -> p C(x y) -> r C(x z))
                      -> IO (Sealed (p C(x))) -> (FORALL(b) IO (Sealed (q C(b))))
                      -> IO (Sealed (r C(x)))
          lift2Sealed f iox ioy = do Sealed x <- unseal seal `fmap`
                                                 unsafeInterleaveIO iox
                                     Sealed y <- unseal seal `fmap`
                                                 unsafeInterleaveIO ioy
                                     return $ seal $ f y x

readTaggedInventory :: Cache -> String -> IO (Maybe String, [(PatchInfo, String)])
readTaggedInventory cache ihash = do
    (fn,i_and_p) <- fetchFileUsingCache cache HashedInventoriesDir ihash
    let i = skipPristine i_and_p
    (rest,str) <- case BC.break ((==)'\n') i of
                  (swt,pistr) | swt == BC.pack "Starting with inventory:" ->
                    case BC.break ((==)'\n') $ B.tail pistr of
                    (h,thisinv) | okayHash hash -> return (Just hash, thisinv)
                                where hash = BC.unpack h
                    _ -> fail $ "Bad hash in file " ++ fn
                  _ -> return (Nothing,i)
    return (rest, readPatchIds str)

copyRepo :: RepoPatch p => Repository p C(r u t) -> RemoteDarcs -> String -> IO ()
copyRepo repo@(Repo outr _ _ _) remote inr = do
    createDirectoryIfMissing False (outr++"/"++darcsdir++"/inventories")
    copyFileOrUrl remote (inr </> darcsdir </> "hashed_inventory")
                         (outr </> darcsdir </> "hashed_inventory")
                  Uncachable -- no need to copy anything but hashed_inventory!
    copySources repo inr
    debugMessage "Done copying hashed inventory."

copySources :: RepoPatch p => Repository p C(r u t) -> String -> IO ()
copySources repo@(Repo outr _ _ _) inr = do
    let repoCache = extractCache $ modifyCache repo dropGlobalCaches
    appendBinFile (outr++"/"++darcsdir++"/prefs/sources") (show $ repo2cache inr `unionCaches` repoCache )
  where
    dropGlobalCaches (Ca cache) = Ca $ filter notGlobalCache cache
    notGlobalCache xs = case xs of
                         Cache DarcsCache.Directory _ _ -> False
                         _                              -> True

writeAndReadPatch :: RepoPatch p => Cache -> Compression -> PatchInfoAnd p C(x y)
                     -> IO (PatchInfoAnd p C(x y))
writeAndReadPatch c compr p =    do (i,h) <- writePatchIfNecesary c compr p
                                    unsafeInterleaveIO $ readp h i
    where parse i h = do debugMessage ("Rereading patch file: "++ show (humanFriendly i))
                         (fn,ps) <- fetchFileUsingCache c HashedPatchesDir h
                         case readPatch ps of
                           Just x -> return x
                           Nothing -> fail $ unlines ["Couldn't parse patch file "++fn,
                                                      "which is",
                                                      renderString $ humanFriendly i]
          readp h i = do Sealed x <- createHashed h (parse i)
                         return $ patchInfoAndPatch i $ unsafeCoerceP x

writeTentativeInventory :: RepoPatch p => Cache -> Compression -> PatchSet p C(Origin x) -> IO ()
writeTentativeInventory cache compr = writeEitherInventory cache compr "tentative_hashed_inventory"

writeEitherInventory :: RepoPatch p => Cache -> Compression -> String -> PatchSet p C(Origin x) -> IO ()
writeEitherInventory cache compr iname x =
    do debugMessage "in writeEitherInventory..."
       createDirectoryIfMissing False $ "_darcs/inventories"
       let k = "Writing inventory"
       beginTedious k
       hsh <- writeInventoryPrivate cache k compr $ slightlyOptimizePatchset x
       endTedious k
       debugMessage "still in writeEitherInventory..."
       case hsh of
         Nothing -> writeBinFile ("_darcs" </> iname) ""
         Just h -> fmap snd (fetchFileUsingCache cache HashedInventoriesDir h)
                   >>= writeAtomicFilePS ("_darcs" </> iname)

writeInventoryPrivate :: RepoPatch p => Cache -> String -> Compression
                        -> PatchSet p C(Origin x) -> IO (Maybe String)
writeInventoryPrivate _ _ _ (PatchSet NilRL NilRL) = return Nothing
writeInventoryPrivate cache _ compr (PatchSet x NilRL) =
  do inventory <- sequence $ mapRL (writePatchIfNecesary cache compr) x
     let inventorylist = hcat (map pihash $ reverse inventory)
     hash <- writeHashFile cache compr HashedInventoriesDir inventorylist
     return $ Just hash
writeInventoryPrivate cache k compr (PatchSet x xs@(Tagged t _ _ :<: _)) =
  do resthash <- write_ts xs
     finishedOneIO k $ maybe "" id resthash
     inventory <- sequence $ mapRL (writePatchIfNecesary cache compr) (x+<+t:<:NilRL)
     let inventorylist = hcat (map pihash $ reverse inventory)
         inventorycontents =
             case resthash of
               Just h -> text ("Starting with inventory:\n"++h) $$ inventorylist
               Nothing -> inventorylist
     hash <- writeHashFile cache compr HashedInventoriesDir inventorycontents
     return $ Just hash
    where write_ts :: RepoPatch p => RL (Tagged p) C(Origin x) -> IO (Maybe String)
          write_ts (Tagged _ (Just h) _ :<: _) = return (Just h) -- already written!
          write_ts (Tagged _ Nothing pps :<: tts) =
              writeInventoryPrivate cache k compr $ PatchSet pps tts
          write_ts NilRL = return Nothing

writePatchIfNecesary :: RepoPatch p => Cache -> Compression
                        -> PatchInfoAnd p C(x y) -> IO (PatchInfo, String)
writePatchIfNecesary c compr hp =
    seq infohp $ case extractHash hp of
                   Right h -> return (infohp, h)
                   Left p -> (\h -> (infohp, h)) `fmap`
                             writeHashFile c compr HashedPatchesDir (showPatch p)
    where infohp = info hp

pihash :: (PatchInfo,String) -> Doc
pihash (pinf,hash) = showPatchInfo pinf $$ text ("hash: " ++ hash ++ "\n")

readInventoryPrivate :: Cache -> String -> String -> IO (Maybe String, [(PatchInfo, String)])
readInventoryPrivate _ d iname = do
    i <- skipPristine `fmap` gzFetchFilePS (d </> iname) Uncachable
    (rest,str) <- case BC.break ((==)'\n') i of
                  (swt,pistr) | swt == BC.pack "Starting with inventory:" ->
                    case BC.break ((==)'\n') $ B.tail pistr of
                    (h,thisinv) | okayHash hash -> return (Just hash, thisinv)
                                where hash = BC.unpack h
                    _ -> fail $ "Bad hash in " ++ toPath d ++ "/_darcs/" ++ iname
                  _ -> return (Nothing, i)
    return (rest, readPatchIds str)

listInventories :: IO [String]
listInventories = do
  x <- fst <$> readInventoryPrivate (Ca []) darcsdir "hashed_inventory"
  case x of
    Nothing -> return []
    Just x' -> f x'
 where
  f i = do
    x <- fst <$> readInventoryPrivate (Ca []) (darcsdir </> "inventories") i
    (i :) <$> case x of
      Nothing -> return []
      Just x' -> f x'

readPatchIds :: B.ByteString -> [(PatchInfo, String)]
readPatchIds inv | B.null inv = []
readPatchIds inv = case parseStrictly readPatchInfo inv of
                     Nothing -> []
                     Just (pinfo,r) ->
                         case readHash r of
                         Nothing -> []
                         Just (h,r') -> (pinfo,h) : readPatchIds r'

readHash :: B.ByteString -> Maybe (String, B.ByteString)
readHash s = let s' = dropSpace s
                 (l,r) = BC.break ((==)'\n') s'
                 (kw,h) = BC.break ((==)' ') l
             in if kw /= BC.pack "hash:" || B.length h <= 1
                then Nothing
                else Just (BC.unpack $ B.tail h,r)

applyPristine :: Patchy q => String -> String -> q C(x y) -> IO ()
applyPristine d iname p =
    do i <- gzReadFilePS (d++"/"++iname)
       h <- applyHashed (inv2pris i) p
       writeDocBinFile (d++"/"++iname) $ pris2inv h i

applyToTentativePristine :: Patchy q => q C(x y) -> IO ()
applyToTentativePristine p = applyPristine "." (darcsdir++"/tentative_pristine") p

copyPristine :: Cache -> Compression -> String -> String -> IO ()
copyPristine c compr d iname = do
    i <- fetchFilePS (d++"/"++iname) Uncachable
    debugMessage $ "Copying hashed pristine tree: "++inv2pris i
    let k = "Copying pristine"
    beginTedious k
    copyHashed k c compr $ inv2pris i
    endTedious k

copyPartialsPristine :: FilePathLike fp =>
                          Cache -> Compression -> String -> String -> [fp] -> IO ()
copyPartialsPristine c compr d iname fps =
  do i <- fetchFilePS (d++"/"++iname) Uncachable
     copyPartialsHashed c compr (inv2pris i) fps

inv2pris :: B.ByteString -> String
inv2pris inv | B.take pristineNameLength inv == pristineName =
                 case takeHash $ B.drop pristineNameLength inv of
                 Just (h,_) -> h
                 Nothing -> error "Bad hash in inventory!"
             | otherwise = sha256sum B.empty

pris2inv :: String -> B.ByteString -> Doc
pris2inv h inv = invisiblePS pristineName <> text h $$ invisiblePS (skipPristine inv)

pristineName :: B.ByteString
pristineName = BC.pack "pristine:"

skipPristine :: B.ByteString -> B.ByteString
skipPristine ps
    | B.take pristineNameLength ps == pristineName = B.drop 1 $ BC.dropWhile (/= '\n') $
                                                        B.drop pristineNameLength ps
    | otherwise = ps

pristineNameLength :: Int
pristineNameLength = B.length pristineName
