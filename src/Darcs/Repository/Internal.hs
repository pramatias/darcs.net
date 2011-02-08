-- Copyright (C) 2002-2004,2007-2008 David Roundy
-- Copyright (C) 2005 Juliusz Chroboczek
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
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

{-# LANGUAGE CPP, ScopedTypeVariables, Rank2Types, RankNTypes, PatternGuards #-}

#include "gadts.h"

module Darcs.Repository.Internal ( Repository(..), RepoType(..), RIO(unsafeUnRIO), RepoJob(..),

                    maybeIdentifyRepository, identifyDarcsRepository, identifyRepositoryFor,
                    IdentifyRepo(..),
                    findRepository, amInRepository, amNotInRepository,
                    revertRepositoryChanges,
                    announceMergeConflicts, setTentativePending,
                    checkUnrecordedConflicts,
                    withRecorded,
                    readRepo, readTentativeRepo,
                    prefsUrl, makePatchLazy,
                    withRepoLock, withRepoReadLock,
                    withRepository, withRepositoryDirectory, withGutsOf,
                    tentativelyAddPatch, tentativelyRemovePatches, tentativelyAddToPending,
                    tentativelyAddPatch_,
                    tentativelyReplacePatches,
                    finalizeRepositoryChanges,
                    unrevertUrl,
                    applyToWorking, patchSetToPatches,
                    createPristineDirectoryTree, createPartialsPristineDirectoryTree,
                    optimizeInventory, cleanRepository,
                    getMarkedupFile,
                    setScriptsExecutable, setScriptsExecutablePatches,
                    getRepository, rIO,
                    testTentative, testRecorded,
                    UpdatePristine(..), MakeChanges(..), applyToTentativePristine,
                    makeNewPending, seekRepo
                  ) where

import Printer ( putDocLn, (<+>), text, ($$) )

import Darcs.Repository.Prefs ( getPrefval )
import Darcs.Repository.State ( readRecorded, readWorking )
import Darcs.Repository.LowLevel
    ( readPending, readTentativePending
    , writeTentativePending
    , readNewPending, writeNewPending
    , pendingName )
import System.Exit ( ExitCode(..), exitWith )
import System.Cmd ( system )
import Darcs.External ( clonePartialsTree )
import Darcs.IO ( runTolerantly, runSilently )
import Darcs.Repository.Pristine ( identifyPristine,
                                   easyCreatePristineDirectoryTree,
                                   easyCreatePartialsPristineDirectoryTree )

import Darcs.SignalHandler ( withSignalsBlocked )
import Darcs.Repository.Format ( RepoFormat, RepoProperty( Darcs2, HashedInventory ),
                                 identifyRepoFormat, formatHas,
                                 writeProblem, readProblem, readfromAndWritetoProblem )
import System.Directory ( doesDirectoryExist, setCurrentDirectory,
                          createDirectoryIfMissing, doesFileExist )
import Control.Monad ( liftM, when, unless, filterM )
import Control.Applicative ( (<$>) )
import Workaround ( getCurrentDirectory, renameFile, setExecutable )

import qualified Data.ByteString as B ( readFile, isPrefixOf )
import qualified Data.ByteString.Char8 as BC (pack)

import Darcs.Patch ( Effect, primIsHunk, primIsBinary, description,
                     tryToShrink, commuteFLorComplain, commute )

import Darcs.Patch.Dummy ( DummyPatch )

import Darcs.Patch.V1 ( Patch )
import Darcs.Patch.V2 ( RealPatch )
import Darcs.Patch.Prim.V1 ( Prim )

import Darcs.Patch.Inspect ( PatchInspect )
import Darcs.Patch.Prim ( PrimPatchBase, PrimOf, tryShrinkingInverse, PrimPatch )
import Darcs.Patch.Bundle ( scanBundle, makeBundleN )
import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info,
                         hopefully, hopefullyM )
import Darcs.Repository.ApplyPatches ( applyPatches )
import qualified Darcs.Repository.HashedRepo as HashedRepo
                            ( revertTentativeChanges, finalizeTentativeChanges,
                              removeFromTentativeInventory,
                              copyPristine, copyPartialsPristine,
                              applyToTentativePristine,
                              writeTentativeInventory, writeAndReadPatch,
                              addToTentativeInventory,
                              readRepo, readTentativeRepo, cleanPristine )
import qualified Darcs.Repository.DarcsRepo as DarcsRepo
import Darcs.Flags ( DarcsFlag(Verbose, Quiet,
                               MarkConflicts, AllowConflicts, NoUpdateWorking,
                               WorkRepoUrl, WorkRepoDir, UMask, Test, LeaveTestDir,
                               SetScriptsExecutable, DryRun ),
                     wantExternalMerge, compression, Compression )
import Darcs.Witnesses.Eq ( EqCheck(..) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP, unsafeCoercePStart )
import Darcs.Witnesses.Ordered ( FL(..), RL(..),
                             (:\/:)(..), (:/\:)(..), (:>)(..),
                             (+>+), lengthFL, dropWhileFL,
                             allFL, filterFLFL,
                             reverseFL, mapFL_FL, concatFL )
import Darcs.Patch ( RepoPatch, Patchy, merge,
                     listConflictedFiles, listTouchedFiles,
                     Named, patchcontents,
                     commuteRL, fromPrims,
                     readPatch,
                     effect, invert,
                     primIsAddfile, primIsAdddir,
                     primIsSetpref,
                     apply, applyToTree,
                     emptyMarkedupFile, MarkedUpFile
                   )
import Darcs.Patch.Permutations ( commuteWhatWeCanFL, removeFL )
import Darcs.Patch.Info ( PatchInfo )
import Darcs.Patch.Set ( PatchSet(..), SealedPatchSet, newset2FL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Patch.Markup ( markupFile )
import Darcs.Patch.MarkupData ( LineMark(None) )
import Darcs.Patch.Depends ( deepOptimizePatchset, removeFromPatchSet, mergeThem )
import Darcs.RepoPath ( FilePathLike, AbsolutePath, toFilePath,
                        ioAbsoluteOrRemote, toPath )
import Darcs.Utils ( promptYorn, catchall, withCurrentDirectory, withUMask, nubsort )
import Progress ( debugMessage )
import Darcs.ProgressPatches (progressFL)
import Darcs.URL ( isFile )
import Darcs.Repository.Prefs ( getCaches )
import Darcs.Lock ( withLock, writeDocBinFile, removeFileMayNotExist,
                    withTempDir, withPermDir )
import Darcs.Witnesses.Sealed ( Sealed(Sealed), seal, FlippedSeal(FlippedSeal), flipSeal )
import Darcs.Repository.InternalTypes( Repository(..), RepoType(..), Pristine(NoPristine) )
import Darcs.Global ( darcsdir )

import System.Mem( performGC )

import qualified Storage.Hashed.Tree as Tree
import Storage.Hashed.AnchoredPath( anchorPath )

#include "impossible.h"

-- | Repository IO monad.  This monad-like datatype is responsible for
-- sequencing IO actions that modify the tentative recorded state of
-- the repository.
newtype RIO p C(r u t t1) a = RIO {
        unsafeUnRIO :: Repository p C(r u t) -> IO a -- ^ converts @RIO a@ to @IO a@.
   }

-- | This is just like @>>=@ from the Monad class except that it
-- respects type witness safe repository transformations.  Even so, it
-- only tracks modifications to the tentative recorded state.
(>>>=) :: RIO p C(r u t t1) a -> (a -> RIO p C(r u t1 t2) b) -> RIO p C(r u t t2) b
m >>>= k = RIO $ \ (Repo x y z w) ->
             do a <- unsafeUnRIO m (Repo x y z w)
                unsafeUnRIO (k a) (Repo x y z w)

-- | This corresponds to @>>@ from the Monad class.
(>>>) :: RIO p C(r u t t1) a -> RIO p C(r u t1 t2) b -> RIO p C(r u t t2) b
a >>> b = a >>>= (const b)

-- | This corresponds to @return@ from the Monad class.
returnR :: a -> RIO p C(r u t t) a
returnR = rIO . return

-- | This the @RIO@ equivalent of @liftIO@.
rIO :: IO a -> RIO p C(r u t t) a
rIO = RIO . const

instance Functor (RIO p C(r u t t)) where
    fmap f m = RIO $ \r -> fmap f (unsafeUnRIO m r)

-- | We have an instance of Monad so that IO actions that do not
-- change the tentative recorded state are convenient in the IO monad.
instance Monad (RIO p C(r u t t)) where
 (>>=) = (>>>=)
 (>>) = (>>>)
 return = returnR
 fail = rIO . fail

-- | Similar to the @ask@ function of the MonadReader class.
-- This allows actions in the RIO monad to get the current
-- repository.
-- FIXME: Don't export this.  If we don't export this
-- it makes it harder for arbitrary IO actions to access
-- the repository and hence our code is easier to audit.
getRepository :: RIO p C(r u t t) (Repository p C(r u t))
getRepository = RIO return

-- | The status of a given directory: is it a darcs repository?
data IdentifyRepo p C(r u t) = BadRepository String -- ^ looks like a repository with some error
                             | NonRepository String -- ^ safest guess
                             | GoodRepository (Repository p C(r u t))

-- | Tries to identify the repository in a given directory
maybeIdentifyRepository :: [DarcsFlag] -> String -> IO (IdentifyRepo p C(r u t))
maybeIdentifyRepository opts "." =
    do darcs <- doesDirectoryExist darcsdir
       rf_or_e <- identifyRepoFormat "."
       here <- toPath `fmap` ioAbsoluteOrRemote "."
       case rf_or_e of
         Left err -> return $ NonRepository err
         Right rf ->
             case readProblem rf of
             Just err -> return $ BadRepository err
             Nothing -> if darcs then do pris <- identifyPristine
                                         cs <- getCaches opts here
                                         return $ GoodRepository $ Repo here opts rf (DarcsRepository pris cs)
                                 else return (NonRepository "Not a repository")
maybeIdentifyRepository opts url' =
 do url <- toPath `fmap` ioAbsoluteOrRemote url'
    rf_or_e <- identifyRepoFormat url
    case rf_or_e of
      Left e -> return $ NonRepository e
      Right rf -> case readProblem rf of
                  Just err -> return $ BadRepository err
                  Nothing ->  do cs <- getCaches opts url
                                 return $ GoodRepository $ Repo url opts rf (DarcsRepository NoPristine cs)

-- | identifyDarcsRepository identifies the repo at 'url'. Warning:
-- you have to know what kind of patches are found in that repo.
identifyDarcsRepository :: forall p C(r u t). [DarcsFlag] -> String
                           -> IO (Repository p C(r u t))
identifyDarcsRepository opts url =
    do er <- maybeIdentifyRepository opts url
       case er of
         BadRepository s -> fail s
         NonRepository s -> fail s
         GoodRepository r -> return r

-- | @identifyRepositoryFor repo url@ identifies (and returns) the repo at 'url',
-- but fails if it is not compatible for reading from and writing to.
identifyRepositoryFor :: forall p C(r u t). RepoPatch p => Repository p C(r u t) -> String -> IO (Repository p C(r u t))
identifyRepositoryFor (Repo _ opts rf _) url =
    do Repo absurl _ rf_ t <- identifyDarcsRepository opts url
       let t' = case t of DarcsRepository x c -> DarcsRepository x c
       case readfromAndWritetoProblem rf_ rf of
         Just e -> fail $ "Incompatibility with repository " ++ url ++ ":\n" ++ e
         Nothing -> return $ Repo absurl opts rf_ t'

amInRepository :: [DarcsFlag] -> IO (Either String ())
amInRepository (WorkRepoDir d:_) =
    do setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       status <- maybeIdentifyRepository [] "."
       case status of
         GoodRepository _ -> return (Right ())
         BadRepository  e -> return (Left $ "While " ++ d ++ " looks like a repository directory, we have a problem with it:\n" ++ e)
         NonRepository  _ -> return (Left "You need to be in a repository directory to run this command.")
amInRepository (_:fs) = amInRepository fs
amInRepository [] = do
    maybe (Left $ "You need to be in a repository directory " ++
        "to run this command.") id <$> seekRepo

-- | hunt upwards for the darcs repository
-- This keeps changing up one parent directory, testing at each
-- step if the current directory is a repository or not.  $
-- The result is:
--   Nothing, if no repository found
--   Just (Left errorMessage), if bad repository found
--   Just (Right ()), if good repository found.
-- WARNING this changes the current directory for good if matchFn succeeds
seekRepo :: IO (Maybe (Either String ()))
seekRepo = getCurrentDirectory >>= helper where
   helper startpwd = do
    status <- maybeIdentifyRepository [] "."
    case status of
      GoodRepository _ -> return . Just $ Right ()
      BadRepository e  -> return . Just $ Left e
      NonRepository _ ->
            do cd <- toFilePath `fmap` getCurrentDirectory
               setCurrentDirectory ".."
               cd' <- toFilePath `fmap` getCurrentDirectory
               if cd' /= cd
                  then helper startpwd
                  else do setCurrentDirectory startpwd
                          return Nothing

-- The performGC in this function is a workaround for a library/GHC bug,
-- http://hackage.haskell.org/trac/ghc/ticket/2924 -- (doesn't seem to be a
-- problem on fast machines, but virtual ones trip this from time to time)
amNotInRepository :: [DarcsFlag] -> IO (Either String ())
amNotInRepository (WorkRepoDir d:_) = do createDirectoryIfMissing False d
                                            `catchall` (performGC >> createDirectoryIfMissing False d)
                                         -- note that the above could always fail
                                         setCurrentDirectory d
                                         amNotInRepository []
amNotInRepository (_:f) = amNotInRepository f
amNotInRepository [] =
    do status <- maybeIdentifyRepository [] "."
       case status of
         GoodRepository _ -> return (Left $ "You may not run this command in a repository.")
         BadRepository e  -> return (Left $ "You may not run this command in a repository.\nBy the way, we have a problem with it:\n" ++ e)
         NonRepository _  -> return (Right ())

findRepository :: [DarcsFlag] -> IO (Either String ())
findRepository (WorkRepoUrl d:_) | isFile d =
    do setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       findRepository []
findRepository (WorkRepoDir d:_) =
    do setCurrentDirectory d `catchall` (fail $ "can't set directory to "++d)
       findRepository []
findRepository (_:fs) = findRepository fs
findRepository [] = maybe (Right ()) id <$> seekRepo

makeNewPending :: forall p C(r u t y). RepoPatch p
                 => Repository p C(r u t) -> FL (PrimOf p) C(t y) -> IO ()
makeNewPending (Repo _ opts _ _) _ | NoUpdateWorking `elem` opts = return ()
makeNewPending repo@(Repo r _ _ tp) origp =
    withCurrentDirectory r $
    do let newname = pendingName tp ++ ".new"
       debugMessage $ "Writing new pending:  " ++ newname
       Sealed sfp <- return $ siftForPending origp
       writeNewPending repo sfp
       cur <- readRecorded repo
       Sealed p <- readNewPending repo :: IO (Sealed (FL (PrimOf p) C(t)))
-- Warning:  A do-notation statement discarded a result of type Tree.Tree IO.
       _ <- catch (applyToTree p cur) $ \err -> do
         let buggyname = pendingName tp ++ "_buggy"
         renameFile newname buggyname
         bugDoc $ text ("There was an attempt to write an invalid pending! " ++ show err)
                    $$ text "If possible, please send the contents of"
                    <+> text buggyname
                    $$ text "along with a bug report."
       renameFile newname (pendingName tp)
       debugMessage $ "Finished writing new pending:  " ++ newname

siftForPending :: forall prim C(x y) . PrimPatch prim => FL prim C(x y) -> Sealed (FL prim C(x))
siftForPending simple_ps =
 let oldps = maybe simple_ps id $ tryShrinkingInverse $ crudeSift simple_ps
 in if allFL (\p -> primIsAddfile p || primIsAdddir p) $ oldps
    then seal oldps
    else fromJust $ do
      Sealed x <- return $ sfp NilFL $ reverseFL oldps
      return (case tryToShrink x of
              ps | lengthFL ps < lengthFL oldps -> siftForPending ps
                 | otherwise -> seal ps)
      where sfp :: FL prim C(a b) -> RL prim C(c a) -> Sealed (FL prim C(c))
            sfp sofar NilRL = seal sofar
            sfp sofar (p:<:ps)
                | primIsHunk p || primIsBinary p
                    = case commuteFLorComplain (p :> sofar) of
                      Right (sofar' :> _) -> sfp sofar' ps
                      Left _ -> sfp (p:>:sofar) ps
            sfp sofar (p:<:ps) = sfp (p:>:sofar) ps

-- @todo: we should not have to open the result of HashedRepo and
-- seal it.  Instead, update this function to work with type witnesses
-- by fixing DarcsRepo to match HashedRepo in the handling of
-- Repository state.
readRepo :: RepoPatch p => Repository p C(r u t) -> IO (PatchSet p C(Origin r))
readRepo repo@(Repo r _ rf _)
    | formatHas HashedInventory rf = HashedRepo.readRepo repo r
    | otherwise = do Sealed ps <- DarcsRepo.readRepo r
                     return $ unsafeCoerceP ps

readTentativeRepo :: RepoPatch p => Repository p C(r u t) -> IO (PatchSet p C(Origin t))
readTentativeRepo repo@(Repo r _ rf _)
    | formatHas HashedInventory rf = HashedRepo.readTentativeRepo repo r
    | otherwise = do Sealed ps <- DarcsRepo.readTentativeRepo r
                     return $ unsafeCoerceP ps

makePatchLazy :: RepoPatch p => Repository p C(r u t) -> PatchInfoAnd p C(x y) -> IO (PatchInfoAnd p C(x y))
makePatchLazy (Repo r opts rf (DarcsRepository _ c)) p
    | formatHas HashedInventory rf = withCurrentDirectory r $ HashedRepo.writeAndReadPatch c (compression opts) p
    | otherwise = withCurrentDirectory r $ DarcsRepo.writeAndReadPatch (compression opts) p

prefsUrl :: Repository p C(r u t) -> String
prefsUrl (Repo r _ _ (DarcsRepository _ _)) = r ++ "/"++darcsdir++"/prefs"

unrevertUrl :: Repository p C(r u t) -> String
unrevertUrl (Repo r _ _ (DarcsRepository _ _)) = r ++ "/"++darcsdir++"/patches/unrevert"

applyToWorking :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> FL (PrimOf p) C(u y) -> IO (Repository p C(r y t))
applyToWorking (Repo r ropts rf (DarcsRepository t c)) opts patch =
    do withCurrentDirectory r $ if Quiet `elem` opts
                                then runSilently $ apply patch
                                else runTolerantly $ apply patch
       return (Repo r ropts rf (DarcsRepository t c))

handlePendForAdd :: forall p C(r u t x y). (RepoPatch p)
                    => Repository p C(r u t) -> PatchInfoAnd p C(x y) -> IO ()
handlePendForAdd (Repo _ opts _ _) _ | NoUpdateWorking `elem` opts = return ()
handlePendForAdd repo p =
    do
       Sealed pend <- readTentativePending repo
       let effectp = if allFL isSimple pend then crudeSift $ effect p
                                             else effect p
       Sealed newpend <- return $ rmpend (progressFL "Removing from pending:" effectp) (unsafeCoercePStart pend)
       writeTentativePending repo (unsafeCoercePStart newpend)
    where rmpend :: FL (PrimOf p) C(a b) -> FL (PrimOf p) C(a c) -> Sealed (FL (PrimOf p) C(b))
          rmpend NilFL x = Sealed x
          rmpend _ NilFL = Sealed NilFL
          rmpend (x:>:xs) xys | Just ys <- removeFL x xys = rmpend xs ys
          rmpend (x:>:xs) ys =
              case commuteWhatWeCanFL (x:>xs) of
              a:>x':>b -> case rmpend a ys of
                          Sealed ys' -> case commute (invert (x':>:b) :> ys') of
                                        Just (ys'' :> _) -> seal ys''
                                        Nothing -> seal $ invert (x':>:b)+>+ys'
                                        -- DJR: I don't think this
                                        -- last case should be
                                        -- reached, but it also
                                        -- shouldn't lead to
                                        -- corruption.

isSimple :: PrimPatch prim => prim C(x y) -> Bool
isSimple x = primIsHunk x || primIsBinary x || primIsSetpref x

crudeSift :: forall prim C(x y) . PrimPatch prim => FL prim C(x y) -> FL prim C(x y)
crudeSift xs = if allFL isSimple xs then filterFLFL ishunkbinary xs else xs
    where ishunkbinary :: prim C(a b) -> EqCheck C(a b)
          ishunkbinary x | primIsHunk x || primIsBinary x = unsafeCoerceP IsEq
                         | otherwise = NotEq

data HashedVsOld a = HvsO { old, hashed :: a }

decideHashedOrNormal :: Monad m => RepoFormat -> HashedVsOld (m a) -> m a
decideHashedOrNormal rf (HvsO { hashed = h, old = o })
    | formatHas HashedInventory rf = h
    | otherwise = o

data MakeChanges = MakeChanges | DontMakeChanges deriving ( Eq )

announceMergeConflicts :: (PrimPatch p, PatchInspect p) => String -> [DarcsFlag] -> FL p C(x y) -> IO Bool
announceMergeConflicts cmd opts resolved_pw =
    case nubsort $ listTouchedFiles resolved_pw of
    [] -> return False
    cfs -> if MarkConflicts `elem` opts || AllowConflicts `elem` opts
              || wantExternalMerge opts /= Nothing
           then do putStrLn "We have conflicts in the following files:"
                   putStrLn $ unwords cfs
                   return True
           else do putStrLn "There are conflicts in the following files:"
                   putStrLn $ unwords cfs
                   fail $ "Refusing to "++cmd++" patches leading to conflicts.\n"++
                          "If you would rather apply the patch and mark the conflicts,\n"++
                          "use the --mark-conflicts or --allow-conflicts options to "++cmd++"\n"++
                          "These can set as defaults by adding\n"++
                          " "++cmd++" mark-conflicts\n"++
                          "to "++darcsdir++"/prefs/defaults in the target repo. "

checkUnrecordedConflicts :: forall p C(t y). RepoPatch p => [DarcsFlag] -> FL (Named p) C(t y) -> IO Bool
checkUnrecordedConflicts opts _ | NoUpdateWorking `elem` opts = return False
checkUnrecordedConflicts opts pc =
    do repository <- identifyDarcsRepository opts "."
       cuc repository
    where cuc :: Repository p C(r u t) -> IO Bool
          cuc r = do Sealed (mpend :: FL (PrimOf p) C(t x)) <- readPending r :: IO (Sealed (FL (PrimOf p) C(t)))
                     case mpend of
                       NilFL -> return False
                       pend ->
                           case merge (fromPrims_ pend :\/: fromPrims_ (concatFL $ mapFL_FL effect pc)) of
                           _ :/\: pend' ->
                               case listConflictedFiles pend' of
                               [] -> return False
                               fs -> do putStrLn ("You have conflicting local changes to:\n"
                                                 ++ unwords fs)
                                        yorn <- promptYorn "Proceed?"
                                        when (yorn /= 'y') $
                                             do putStrLn "Cancelled."
                                                exitWith ExitSuccess
                                        return True
          fromPrims_ :: FL (PrimOf p) C(a b) -> FL p C(a b)
          fromPrims_ = fromPrims

tentativelyAddPatch :: RepoPatch p
                    => Repository p C(r u t) -> Compression -> PatchInfoAnd p C(t y) -> IO (Repository p C(r u y))
tentativelyAddPatch = tentativelyAddPatch_ UpdatePristine

data UpdatePristine = UpdatePristine | DontUpdatePristine deriving Eq

-- TODO re-add a safety catch for --dry-run? Maybe using a global, like dryRun
-- :: Bool, with dryRun = unsafePerformIO $ readIORef ...
tentativelyAddPatch_ :: RepoPatch p
                     => UpdatePristine -> Repository p C(r u t) -> Compression
                     -> PatchInfoAnd p C(t y) -> IO (Repository p C(r u y))
tentativelyAddPatch_ up r@(Repo dir ropts rf (DarcsRepository t c)) compr p =
    withCurrentDirectory dir $
-- Warning:  A do-notation statement discarded a result of type FilePath.
    do _ <- decideHashedOrNormal rf $ HvsO {
          hashed = HashedRepo.addToTentativeInventory c compr p,
          old = DarcsRepo.addToTentativeInventory compr (hopefully p) }
       when (up == UpdatePristine) $ do debugMessage "Applying to pristine cache..."
                                        applyToTentativePristine r p
                                        debugMessage "Updating pending..."
                                        handlePendForAdd r p
       return (Repo dir ropts rf (DarcsRepository t c))

applyToTentativePristine :: (Effect q, Patchy q, PrimPatchBase q) => Repository p C(r u t) -> q C(t y) -> IO ()
applyToTentativePristine (Repo dir opts rf (DarcsRepository _ _)) p =
    withCurrentDirectory dir $
    do when (Verbose `elem` opts) $ putDocLn $ text "Applying to pristine..." <+> description p
       decideHashedOrNormal rf $ HvsO {hashed = HashedRepo.applyToTentativePristine p,
                                       old = DarcsRepo.addToTentativePristine p}

-- | This fuction is unsafe because it accepts a patch that works on the tentative
-- pending and we don't currently track the state of the tentative pending.
tentativelyAddToPending :: forall p C(r u t x y). RepoPatch p
                        => Repository p C(r u t) -> [DarcsFlag] -> FL (PrimOf p) C(x y) -> IO ()
tentativelyAddToPending (Repo _ opts _ _) _ _
    | NoUpdateWorking `elem` opts = return ()
    | DryRun `elem` opts = bug "tentativelyAddToPending called when --dry-run is specified"
tentativelyAddToPending repo@(Repo dir _ _ _) _ patch =
    withCurrentDirectory dir $ do
      Sealed pend <- readTentativePending repo
      FlippedSeal newpend_ <- return $ newpend (unsafeCoerceP pend :: FL (PrimOf p) C(a x)) patch
      writeTentativePending repo (unsafeCoercePStart newpend_)
      where newpend :: FL prim C(a b) -> FL prim C(b c) -> FlippedSeal (FL prim) C(c)
            newpend NilFL patch_ = flipSeal patch_
            newpend p     patch_ = flipSeal $ p +>+ patch_

-- | setTentativePending is basically unsafe.  It overwrites the pending state with a new one, not related to
-- the repository state.
setTentativePending :: forall p C(r u t x y). RepoPatch p => Repository p C(r u t) -> FL (PrimOf p) C(x y) -> IO ()
setTentativePending (Repo _ opts _ _) _ | NoUpdateWorking `elem` opts = return ()
setTentativePending repo@(Repo dir _ _ _) patch = do
    Sealed prims <- return $ siftForPending patch
    withCurrentDirectory dir $ writeTentativePending repo (unsafeCoercePStart prims)

-- | prepend is basically unsafe.  It overwrites the pending state
-- with a new one, not related to the repository state.
prepend :: forall p C(r u t x y). RepoPatch p =>
           Repository p C(r u t) -> FL (PrimOf p) C(x y) -> IO ()
prepend (Repo _ opts _ _) _ | NoUpdateWorking `elem` opts = return ()
prepend repo@(Repo _ _ _ _) patch =
    do
       Sealed pend <- readTentativePending repo
       Sealed newpend_ <- return $ newpend (unsafeCoerceP pend) patch
       writeTentativePending repo (unsafeCoercePStart $ crudeSift newpend_)
      where newpend :: FL prim C(b c) -> FL prim C(a b) -> Sealed (FL prim C(a))
            newpend NilFL patch_ = seal patch_
            newpend p     patch_ = seal $ patch_ +>+ p

tentativelyRemovePatches :: RepoPatch p => Repository p C(r u t) -> Compression
                         -> FL (PatchInfoAnd p) C(x t) -> IO (Repository p C(r u x))
tentativelyRemovePatches = tentativelyRemovePatches_ UpdatePristine

tentativelyRemovePatches_ :: forall p C(r u t x). RepoPatch p => UpdatePristine
                          -> Repository p C(r u t) -> Compression
                          -> FL (PatchInfoAnd p) C(x t) -> IO (Repository p C(r u x))
tentativelyRemovePatches_ up repository@(Repo dir ropts rf (DarcsRepository t c)) compr ps =
    withCurrentDirectory dir $ do
      when (up == UpdatePristine) $ do debugMessage "Adding changes to pending..."
                                       prepend repository $ effect ps
      removeFromUnrevertContext repository ps
      debugMessage "Removing changes from tentative inventory..."
      if formatHas HashedInventory rf
        then do HashedRepo.removeFromTentativeInventory repository compr ps
                when (up == UpdatePristine) $
                     HashedRepo.applyToTentativePristine $
                     progressFL "Applying inverse to pristine" $ invert ps
        else DarcsRepo.removeFromTentativeInventory (up==UpdatePristine) compr ps
      return (Repo dir ropts rf (DarcsRepository t c))

tentativelyReplacePatches :: forall p C(r u t x). RepoPatch p => Repository p C(r u t) -> Compression
                          -> FL (PatchInfoAnd p) C(x t) -> IO (Repository p C(r u t))
tentativelyReplacePatches repository compr ps =
    do repository' <- tentativelyRemovePatches_ DontUpdatePristine repository compr ps
       mapAdd repository' ps
  where mapAdd :: Repository p C(m l i) -> FL (PatchInfoAnd p) C(i j) -> IO (Repository p C(m l j))
        mapAdd r NilFL = return r
        mapAdd r (a:>:as) =
               do r' <- tentativelyAddPatch_ DontUpdatePristine r compr a
                  mapAdd r' as

finalizePending :: RepoPatch p => Repository p C(r u t) -> IO ()
finalizePending (Repo dir opts _ rt)
    | NoUpdateWorking `elem` opts =
        withCurrentDirectory dir $ removeFileMayNotExist $ (pendingName rt)
finalizePending repository@(Repo dir _ _ _) = do
  withCurrentDirectory dir $ do
                                Sealed tpend <- readTentativePending repository
                                Sealed new_pending <- return $ siftForPending tpend
                                makeNewPending repository new_pending

finalizeRepositoryChanges :: RepoPatch p => Repository p C(r u t) -> IO ()
finalizeRepositoryChanges (Repo _ opts _ _)
    | DryRun `elem` opts = bug "finalizeRepositoryChanges called when --dry-run specified"
finalizeRepositoryChanges repository@(Repo dir opts rf _)
    | formatHas HashedInventory rf =
        withCurrentDirectory dir $ do debugMessage "Finalizing changes..."
                                      withSignalsBlocked $ do HashedRepo.finalizeTentativeChanges repository (compression opts)
                                                              finalizePending repository
                                      debugMessage "Done finalizing changes..."
finalizeRepositoryChanges repository@(Repo dir _ _ (DarcsRepository _ _)) =
  withCurrentDirectory dir $ do debugMessage "Finalizing changes..."
                                withSignalsBlocked $ do DarcsRepo.finalizePristineChanges
                                                        DarcsRepo.finalizeTentativeChanges
                                                        finalizePending repository


testTentative :: RepoPatch p => Repository p C(r u t) -> IO (ExitCode)
testTentative = testAny withTentative

testRecorded :: RepoPatch p => Repository p C(r u t) -> IO (ExitCode)
testRecorded = testAny withRecorded

testAny :: RepoPatch p => (Repository p C(r u t)
                               -> ((AbsolutePath -> IO (ExitCode)) -> IO (ExitCode))
                               -> ((AbsolutePath -> IO (ExitCode)) -> IO (ExitCode)))
        ->  Repository p C(r u t) -> IO (ExitCode)
testAny withD repository@(Repo dir opts _ _) =
    debugMessage "Considering whether to test..." >>
    if not $ Test `elem` opts then return ExitSuccess else withCurrentDirectory dir $
    do let putInfo = if Quiet `elem` opts then const (return ()) else putStrLn
       debugMessage "About to run test if it exists."
       testline <- getPrefval "test"
       case testline of
         Nothing -> return ExitSuccess
         Just testcode ->
             withD repository (wd "testing") $ \_ ->
             do putInfo "Running test...\n"
                when (SetScriptsExecutable `elem` opts) setScriptsExecutable
                ec <- system testcode
                if ec == ExitSuccess
                  then putInfo "Test ran successfully.\n"
                  else putInfo "Test failed!\n"
                return ec
    where wd = if LeaveTestDir `elem` opts then withPermDir else withTempDir

revertRepositoryChanges :: RepoPatch p => Repository p C(r u t) -> IO ()
revertRepositoryChanges (Repo _ opts _ _)
    | DryRun `elem` opts = bug "revertRepositoryChanges called when --dry-run is specified"
revertRepositoryChanges r@(Repo dir opts rf dr@(DarcsRepository _ _)) =
    withCurrentDirectory dir $
    do removeFileMayNotExist (pendingName dr ++ ".tentative")
       Sealed x <- readPending r
       setTentativePending r x
       when (NoUpdateWorking `elem` opts) $ removeFileMayNotExist $ pendingName dr
       decideHashedOrNormal rf $ HvsO { hashed = HashedRepo.revertTentativeChanges,
                                        old = DarcsRepo.revertTentativeChanges }

patchSetToPatches :: RepoPatch p => PatchSet p C(x y) -> FL (Named p) C(x y)
patchSetToPatches patchSet = mapFL_FL hopefully $ newset2FL patchSet

getUMask :: [DarcsFlag] -> Maybe String
getUMask [] = Nothing
getUMask ((UMask u):_) = Just u
getUMask (_:l) = getUMask l

withUMaskFromOpts :: [DarcsFlag] -> IO a -> IO a
withUMaskFromOpts = maybe id withUMask . getUMask

withGutsOf :: Repository p C(r u t) -> IO () -> IO ()
withGutsOf (Repo _ _ rf _) | formatHas HashedInventory rf = id
                           | otherwise = withSignalsBlocked

data RepoJob a
    = RepoJob (forall p C(r u) . RepoPatch p => Repository p C(r u r) -> IO a)
    | V1Job (forall C(r u) . Repository (Patch Prim) C(r u r) -> IO a)
    | V2Job (forall C(r u) . Repository (RealPatch Prim) C(r u r) -> IO a)

onRepoJob :: RepoJob a
          -> (forall p C(r u) . RepoPatch p => (Repository p C(r u r) -> IO a) -> (Repository p C(r u r) -> IO a))
          -> RepoJob a
onRepoJob (RepoJob job) f = RepoJob (f job)
onRepoJob (V1Job job) f = V1Job (f job)
onRepoJob (V2Job job) f = V2Job (f job)

withRepository :: [DarcsFlag] -> RepoJob a -> IO a
withRepository opts1 = withRepositoryDirectory opts1 "."

withRepositoryDirectory :: forall a. [DarcsFlag] -> String -> RepoJob a -> IO a
withRepositoryDirectory opts1 url repojob =
    do Repo dir opts rf (DarcsRepository t c) <- identifyDarcsRepository opts1 url
       if formatHas Darcs2 rf
         then do debugMessage $ "Identified darcs-2 repo: " ++ dir
                 let therepo = Repo dir opts rf (DarcsRepository t c) :: Repository (RealPatch Prim) C(r u r)
                 case repojob of
                   RepoJob job -> job therepo
                   V2Job job -> job therepo
                   V1Job _ -> fail "This repository contains darcs v1 patches, but the command requires darcs v2 patches."
         else do debugMessage $ "Identified darcs-1 repo: " ++ dir
                 let therepo = Repo dir opts rf (DarcsRepository t c) :: Repository (Patch Prim) C(r u r)
                 case repojob of
                   RepoJob job -> job therepo
                   V1Job job -> job therepo
                   V2Job _ -> fail "This repository contains darcs v2 patches, but the command requires darcs v1 patches."

withRepoLock :: [DarcsFlag] -> RepoJob a -> IO a
withRepoLock opts repojob =
    withRepository opts $ onRepoJob repojob $ \job repository@(Repo _ _ rf _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFromOpts opts $ if DryRun `elem` opts
                                then job repository
                                else withLock name (revertRepositoryChanges repository >> job repository)

withRepoReadLock :: [DarcsFlag] -> RepoJob a -> IO a
withRepoReadLock opts repojob =
    withRepository opts $ onRepoJob repojob $ \job repository@(Repo _ _ rf _) ->
    do maybe (return ()) fail $ writeProblem rf
       let name = "./"++darcsdir++"/lock"
       withUMaskFromOpts opts $ if formatHas HashedInventory rf || DryRun `elem` opts
                                then job repository
                                else withLock name (revertRepositoryChanges repository >> job repository)

removeFromUnrevertContext :: forall p C(r u t x). RepoPatch p
                             => Repository p C(r u t) -> FL (PatchInfoAnd p) C(x t) -> IO ()
removeFromUnrevertContext repository ps = do
  Sealed bundle <- unrevert_patch_bundle `catchall` (return $ seal (PatchSet NilRL NilRL))
  remove_from_unrevert_context_ bundle
  where unrevert_impossible =
            do yorn <- promptYorn "This operation will make unrevert impossible!\nProceed?"
               case yorn of
                 'n' -> fail "Cancelled."
                 'y' -> removeFileMayNotExist (unrevertUrl repository)
                 _ -> impossible
        unrevert_patch_bundle :: IO (SealedPatchSet p C(Origin))
        unrevert_patch_bundle = do pf <- B.readFile (unrevertUrl repository)
                                   case scanBundle pf of
                                     Right foo -> return foo
                                     Left err -> fail $ "Couldn't parse unrevert patch:\n" ++ err
        remove_from_unrevert_context_ :: PatchSet p C(Origin z) -> IO ()
        remove_from_unrevert_context_ (PatchSet NilRL NilRL) = return ()
        remove_from_unrevert_context_ bundle =
         do debugMessage "Adjusting the context of the unrevert changes..."
            debugMessage $ "Removing "++ show (lengthFL ps) ++
                                  " patches in removeFromUnrevertContext!"
            ref <- readTentativeRepo repository
            let withSinglet :: Sealed (FL ppp C(xxx))
                            -> (FORALL(yyy) ppp C(xxx yyy) -> IO ()) -> IO ()
                withSinglet (Sealed (x :>: NilFL)) j = j x
                withSinglet _ _ = return ()
            withSinglet (mergeThem ref bundle) $ \h_us ->
                  case commuteRL (reverseFL ps :> h_us) of
                    Nothing -> unrevert_impossible
                    Just (us' :> _) ->
                      case removeFromPatchSet ps ref of
                      Nothing -> unrevert_impossible
                      Just common ->
                          do debugMessage "Have now found the new context..."
                             bundle' <- makeBundleN Nothing common (hopefully us':>:NilFL)
                             writeDocBinFile (unrevertUrl repository) bundle'
            debugMessage "Done adjusting the context of the unrevert changes!"

-- | Writes out a fresh copy of the inventory that minimizes the
-- amount of inventory that need be downloaded when people pull from
-- the repository.
--
-- Specifically, it breaks up the inventory on the most recent tag.
-- This speeds up most commands when run remotely, both because a
-- smaller file needs to be transfered (only the most recent
-- inventory).  It also gives a guarantee that all the patches prior
-- to a given tag are included in that tag, so less commutation and
-- history traversal is needed.  This latter issue can become very
-- important in large repositories.
optimizeInventory :: RepoPatch p => Repository p C(r u t) -> IO ()
optimizeInventory repository@(Repo r opts rf (DarcsRepository _ c)) =
    do ps <- readRepo repository
       decideHashedOrNormal rf $
           HvsO { hashed = do revertRepositoryChanges repository
                              HashedRepo.writeTentativeInventory c (compression opts) $ deepOptimizePatchset ps
                              finalizeRepositoryChanges repository,
                  old = DarcsRepo.writeInventory r $ deepOptimizePatchset ps
                }

cleanRepository :: RepoPatch p => Repository p C(r u t) -> IO ()
cleanRepository repository@(Repo _ _ rf _) =
    decideHashedOrNormal rf $
    HvsO { hashed = HashedRepo.cleanPristine repository,
           old = return () }

createPristineDirectoryTree :: RepoPatch p => Repository p C(r u t) -> FilePath -> IO ()
createPristineDirectoryTree repo@(Repo r opts rf (DarcsRepository pris c)) reldir
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True reldir
           withCurrentDirectory reldir $ HashedRepo.copyPristine c (compression opts) r (darcsdir++"/hashed_inventory")
    | otherwise =
        do dir <- toPath `fmap` ioAbsoluteOrRemote reldir
           done <- withCurrentDirectory r $ easyCreatePristineDirectoryTree pris dir
           unless done $ do Sealed patches <- (seal . newset2FL) `liftM` readRepo repo
                            createDirectoryIfMissing True dir
                            withCurrentDirectory dir $ applyPatches patches

-- fp below really should be FileName
-- | Used by the commands dist and diff
createPartialsPristineDirectoryTree :: (FilePathLike fp, RepoPatch p) => Repository p C(r u t) -> [fp] -> FilePath -> IO ()
createPartialsPristineDirectoryTree (Repo r opts rf (DarcsRepository _ c)) prefs dir
    | formatHas HashedInventory rf =
        do createDirectoryIfMissing True dir
           withCurrentDirectory dir $
               HashedRepo.copyPartialsPristine c (compression opts) r (darcsdir++"/hashed_inventory") prefs
createPartialsPristineDirectoryTree r@(Repo rdir _ _ (DarcsRepository pris _)) prefs dir
 = withCurrentDirectory rdir $
   do done <- easyCreatePartialsPristineDirectoryTree prefs pris dir
      unless done $ withRecorded r (withTempDir "recorded") $ \_ -> do
          clonePartialsTree "." dir (map toFilePath prefs)

withRecorded :: RepoPatch p => Repository p C(r u t)
             -> ((AbsolutePath -> IO a) -> IO a) -> (AbsolutePath -> IO a) -> IO a
withRecorded repository mk_dir f
    = mk_dir $ \d -> do createPristineDirectoryTree repository (toFilePath d)
                        f d

withTentative :: forall p a C(r u t). RepoPatch p =>
                 Repository p C(r u t) -> ((AbsolutePath -> IO a) -> IO a)
              -> (AbsolutePath -> IO a) -> IO a
withTentative (Repo dir opts rf (DarcsRepository _ c)) mk_dir f
    | formatHas HashedInventory rf =
        mk_dir $ \d -> do HashedRepo.copyPristine c (compression opts) dir (darcsdir++"/tentative_pristine")
                          f d
withTentative repository@(Repo dir _ _ _) mk_dir f =
    withRecorded repository mk_dir $ \d ->
    do Sealed ps <- read_patches (dir ++ "/"++darcsdir++"/tentative_pristine")
       apply ps
       f d
    where read_patches :: FilePath -> IO (Sealed (FL p C(x)))
          read_patches fil = do ps <- B.readFile fil
                                return $ maybe (seal NilFL) id $ readPatch ps

getMarkedupFile :: RepoPatch p => Repository p C(r u t) -> PatchInfo -> FilePath -> IO (MarkedUpFile PatchInfo)
getMarkedupFile repository pinfo f = do
  Sealed (FlippedSeal patches) <- (seal . dropWhileFL ((/= pinfo) . info)
                                  . newset2FL) `liftM` readRepo repository
  return $ snd $ doMarkAll patches (f, emptyMarkedupFile)

doMarkAll :: RepoPatch p => FL (PatchInfoAnd p) C(x y)
            -> (FilePath, MarkedUpFile PatchInfo) -> (FilePath, MarkedUpFile PatchInfo)
doMarkAll (hp:>:pps) (f, mk) =
    case hopefullyM hp of
    Just p -> doMarkAll pps $ markupFile (info hp) (patchcontents p) (f, mk)
    Nothing -> (f, [(BC.pack "Error reading a patch!",None)])
doMarkAll NilFL (f, mk) = (f, mk)

-- | Sets scripts in or below the current directory executable. A script is any file that starts
--   with the bytes '#!'. This is used for --set-scripts-executable.
setScriptsExecutable_ :: Patchy p => Maybe (p C(x y)) -> IO ()
setScriptsExecutable_ pw = do
    debugMessage "Making scripts executable"
    tree <- readWorking
    paths <- case pw of
          Just ps -> filterM doesFileExist $ listTouchedFiles ps
          Nothing -> return [ anchorPath "." p | (p, Tree.File _) <- Tree.list tree ]
    let setExecutableIfScript f =
              do contents <- B.readFile f
                 when (BC.pack "#!" `B.isPrefixOf` contents) $ do
                   debugMessage ("Making executable: " ++ f)
                   setExecutable f True
    mapM_ setExecutableIfScript paths


setScriptsExecutable :: IO ()
setScriptsExecutable = setScriptsExecutable_ (Nothing :: Maybe (FL DummyPatch C(x y)))

setScriptsExecutablePatches :: Patchy p => p C(x y) -> IO ()
setScriptsExecutablePatches = setScriptsExecutable_ . Just
