-- Copyright (C) 2002-2003,2007 David Roundy
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

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP, FlexibleInstances, TypeSynonymInstances #-}

#include "gadts.h"

module Darcs.Test.Patch.Test
             (
               Check, checkPatch, checkAPatch, verboseCheckAPatch
             ) where

import Prelude hiding ( pi )
import System.IO.Unsafe ( unsafePerformIO )
import Test.QuickCheck
import Control.Applicative
import Control.Monad ( liftM, liftM2, liftM3, liftM4, replicateM )

import Darcs.Patch.Info ( PatchInfo, patchinfo )
import Darcs.Test.Patch.Check ( PatchCheck,
                                checkMove, removeDir, createDir,
                                isValid, insertLine, fileEmpty, fileExists,
                                deleteLine, modifyFile, createFile, removeFile,
                                doCheck, doVerboseCheck, FileContents(..)
                              )
import Darcs.Patch.RegChars ( regChars )
import ByteStringUtils ( linesPS )
import qualified Data.ByteString as B ( ByteString, null, concat )
import qualified Data.ByteString.Char8 as BC ( break, pack )
import Darcs.Patch.FileName ( fn2fp )
import qualified Data.Map as M ( mapMaybe )

import Darcs.Patch ( addfile, adddir, move,
                     hunk, tokreplace, binary,
                     changepref, invert, merge,
                     effect )
import Darcs.Patch.Invert ( Invert )
import Darcs.Patch.V1 ()
import qualified Darcs.Patch.V1.Core as V1 ( Patch(..) )
import Darcs.Patch.V1.Core ( isMerger )
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( Prim(..), DirPatchType(..), FilePatchType(..) )
import Darcs.Witnesses.Ordered
import Darcs.Witnesses.Sealed ( Sealed(Sealed), unseal, mapSeal, Sealed2(..) )
import Darcs.Witnesses.Unsafe

#include "impossible.h"

type Patch = V1.Patch Prim

class ArbitraryP p where
    arbitraryP :: Gen (Sealed (p C(x)))

{-
TODO: there is a lot of overlap in testing between between this module
and Darcs.Test.Patch.QuickCheck

This module tests Prim and V1 patches, and Darcs.Test.Patch.QuickCheck
tests Prim and V2 patches

This module's generator covers a wider set of patch types, but is less
likely to generate conflicts than Darcs.Test.Patch.QuickCheck.

Until this is cleaned up, we take some care that the Arbitrary instances
do not overlap and are only used for tests from the respective
modules.

(There are also tests in other modules that probably depend on the
Arbitrary instances in this module.)
-}

instance Arbitrary (Sealed (Prim C(x))) where
    arbitrary = arbitraryP

instance Arbitrary (Sealed (FL Patch C(x))) where
    arbitrary = arbitraryP

instance Arbitrary (Sealed2 (Prim :> Prim)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :\/: FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :> FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP

instance Arbitrary (Sealed2 (FL Patch :> FL Patch :> FL Patch)) where
    arbitrary = unseal Sealed2 <$> arbitraryP


instance (ArbitraryP p1, ArbitraryP p2) => ArbitraryP (p1 :> p2) where
    arbitraryP = do Sealed p1 <- arbitraryP
                    Sealed p2 <- arbitraryP
                    return (Sealed (p1 :> p2))

instance (ArbitraryP p1, ArbitraryP p2) => ArbitraryP (p1 :\/: p2) where
    arbitraryP = do Sealed p1 <- arbitraryP
                    Sealed p2 <- arbitraryP
                    return (Sealed (unsafeCoercePEnd p1 :\/: p2))

instance ArbitraryP (FL Patch) where
    arbitraryP = sized arbpatch

instance ArbitraryP Prim where
    arbitraryP = onepatchgen

hunkgen :: Gen (Sealed (Prim C(x)))
hunkgen = do
  i <- frequency [(1,choose (0,5)),(1,choose (0,35)),
                  (2,return 0),(3,return 1),(2,return 2),(1,return 3)]
  j <- frequency [(1,choose (0,5)),(1,choose (0,35)),
                  (2,return 0),(3,return 1),(2,return 2),(1,return 3)]
  if i == 0 && j == 0 then hunkgen
    else Sealed <$>
            liftM4 hunk filepathgen linenumgen
                (replicateM i filelinegen)
                (replicateM j filelinegen)

tokreplacegen :: Gen (Sealed (Prim C(x)))
tokreplacegen = do
  f <- filepathgen
  o <- tokengen
  n <- tokengen
  if o == n
     then return $ Sealed $ tokreplace f "A-Za-z" "old" "new"
     else return $ Sealed $ tokreplace f "A-Za-z_" o n

twofilegen :: (FORALL(y) FilePath -> FilePath -> Prim C(x y)) -> Gen (Sealed (Prim C(x)))
twofilegen p = do
  n1 <- filepathgen
  n2 <- filepathgen
  if n1 /= n2 && checkAPatch (p n1 n2)
     then return $ Sealed $ p n1 n2
     else twofilegen p

chprefgen :: Gen (Sealed (Prim C(x)))
chprefgen = do
  f <- oneof [return "color", return "movie"]
  o <- tokengen
  n <- tokengen
  if o == n then return $ Sealed $ changepref f "old" "new"
            else return $ Sealed $ changepref f o n

simplepatchgen :: Gen (Sealed (Prim C(x)))
simplepatchgen = frequency [(1,liftM (Sealed . addfile) filepathgen),
                            (1,liftM (Sealed . adddir) filepathgen),
                            (1,liftM3 (\x y z -> Sealed (binary x y z)) filepathgen arbitrary arbitrary),
                            (1,twofilegen move),
                            (1,tokreplacegen),
                            (1,chprefgen),
                            (7,hunkgen)
                           ]

onepatchgen :: Gen (Sealed (Prim C(x)))
onepatchgen = oneof [simplepatchgen, mapSeal (invert . unsafeCoerceP) `fmap` simplepatchgen]

norecursgen :: Int -> Gen (Sealed (FL Patch C(x)))
norecursgen 0 = mapSeal (\p -> V1.PP p :>: NilFL) `fmap` onepatchgen
norecursgen n = oneof [mapSeal (\p -> V1.PP p :>: NilFL) `fmap` onepatchgen,flatcompgen n]

arbpatch :: Int -> Gen (Sealed (FL Patch C(x)))
arbpatch 0 = mapSeal (\p -> V1.PP p :>: NilFL) `fmap` onepatchgen
arbpatch n = frequency [(3,mapSeal (\p -> V1.PP p :>: NilFL) `fmap` onepatchgen),
                        (2,flatcompgen n),
                        (0,rawMergeGen n),
                        (0,mergegen n),
                        (1,mapSeal (\p -> V1.PP p :>: NilFL) `fmap` onepatchgen)
                       ]

-- | Generate an arbitrary list of at least one element
unempty :: Arbitrary a => Gen [a]
unempty = do
  a <- arbitrary
  as <- arbitrary
  return (a:as)

rawMergeGen :: Int -> Gen (Sealed (FL Patch C(x)))
rawMergeGen n =   do Sealed p1 <- arbpatch len
                     Sealed p2 <- arbpatch len
                     if checkAPatch (invert p1:>:p2:>:NilFL) &&
                        checkAPatch (invert p2:>:p1:>:NilFL)
                        then case merge (p2 :\/: p1) of
                             _ :/\: p2' -> return (Sealed (unsafeCoercePStart p2'))
                        else rawMergeGen n
    where len = if n < 15 then n`div`3 else 3

mergegen :: Int -> Gen (Sealed (FL Patch C(x)))
mergegen n = do
  Sealed p1 <- norecursgen len
  Sealed p2 <- norecursgen len
  if checkAPatch (invert p1:>:p2:>:NilFL) &&
         checkAPatch (invert p2:>:p1:>:NilFL)
     then case merge (p2:\/:p1) of
          _ :/\: p2' ->
              if checkAPatch (p1+>+p2')
              then return $ Sealed $ p1+>+p2'
              else impossible
     else mergegen n
  where len = if n < 15 then n`div`3 else 3

arbpi :: Gen PatchInfo
arbpi = do n <- unempty
           a <- unempty
           l <- unempty
           d <- unempty
           return $ unsafePerformIO $ patchinfo n d a l

instance Arbitrary PatchInfo where
    arbitrary = arbpi

instance Arbitrary B.ByteString where
    arbitrary = liftM BC.pack arbitrary

flatlistgen :: Int -> Gen (Sealed (FL Patch C(x)))
flatlistgen 0 = return $ Sealed NilFL
flatlistgen n = do Sealed x <- onepatchgen
                   Sealed xs <- flatlistgen (n-1)
                   return (Sealed (V1.PP x :>: xs))

flatcompgen :: Int -> Gen (Sealed (FL Patch C(x)))
flatcompgen n = do
  Sealed ps <- flatlistgen n
  let myp = regularizePatches $ ps
  if checkAPatch myp
     then return $ Sealed myp
     else flatcompgen n

-- resize to size 25, that means we'll get line numbers no greater
-- than 1025 using QuickCheck 2.1
linenumgen :: Gen Int
linenumgen = frequency [(1,return 1), (1,return 2), (1,return 3),
                    (3,liftM (\n->1+abs n) (resize 25 arbitrary)) ]

tokengen :: Gen String
tokengen = oneof [return "hello", return "world", return "this",
                  return "is", return "a", return "silly",
                  return "token", return "test"]

toklinegen :: Gen String
toklinegen = liftM unwords $ replicateM 3 tokengen

filelinegen :: Gen B.ByteString
filelinegen = liftM BC.pack $
              frequency [(1,map fromSafeChar `fmap` arbitrary),(5,toklinegen),
                         (1,return ""), (1,return "{"), (1,return "}") ]

filepathgen :: Gen String
filepathgen = liftM fixpath badfpgen

fixpath :: String -> String
fixpath "" = "test"
fixpath p = fpth p

fpth :: String -> String
fpth ('/':'/':cs) = fpth ('/':cs)
fpth (c:cs) = c : fpth cs
fpth [] = []

newtype SafeChar = SS Char
instance Arbitrary SafeChar where
    arbitrary = oneof $ map (return . SS) (['a'..'z']++['A'..'Z']++['1'..'9']++"0")

fromSafeChar :: SafeChar -> Char
fromSafeChar (SS s) = s

badfpgen :: Gen String
badfpgen =  frequency [(1,return "test"), (1,return "hello"), (1,return "world"),
                       (1,map fromSafeChar `fmap` arbitrary),
                       (1,liftM2 (\a b-> a++"/"++b) filepathgen filepathgen) ]

class Check p where
   checkPatch :: p C(x y) -> PatchCheck Bool

instance Check p => Check (FL p) where
   checkPatch NilFL = isValid
   checkPatch (p :>: ps) = checkPatch p >> checkPatch ps

checkAPatch :: (Invert p, Check p) => p C(x y) -> Bool
checkAPatch p = doCheck $ do _ <- checkPatch p
                             checkPatch $ invert p

verboseCheckAPatch :: (Invert p, Check p) => p C(x y) -> Bool
verboseCheckAPatch p = doVerboseCheck $ do checkPatch p

instance Check Patch where
   checkPatch p | isMerger p = do
     checkPatch $ effect p
   checkPatch (V1.Merger _ _ _ _) = impossible
   checkPatch (V1.Regrem _ _ _ _) = impossible
   checkPatch (V1.PP p) = checkPatch p

instance Check Prim where

   checkPatch (FP f RmFile) = removeFile $ fn2fp f
   checkPatch (FP f AddFile) =  createFile $ fn2fp f
   checkPatch (FP f (Hunk line old new)) = do
       _ <- fileExists $ fn2fp f
       mapM_ (deleteLine (fn2fp f) line) old
       mapM_ (insertLine (fn2fp f) line) (reverse new)
       isValid
   checkPatch (FP f (TokReplace t old new)) =
       modifyFile (fn2fp f) (tryTokPossibly t old new)
   -- note that the above isn't really a sure check, as it leaves PSomethings
   -- and PNothings which may have contained new...
   checkPatch (FP f (Binary o n)) = do
       _ <- fileExists $ fn2fp f
       mapM_ (deleteLine (fn2fp f) 1) (linesPS o)
       _ <- fileEmpty $ fn2fp f
       mapM_ (insertLine (fn2fp f) 1) (reverse $ linesPS n)
       isValid

   checkPatch (DP d AddDir) = createDir $ fn2fp d
   checkPatch (DP d RmDir) = removeDir $ fn2fp d

   checkPatch (Move f f') = checkMove (fn2fp f) (fn2fp f')
   checkPatch (ChangePref _ _ _) = return True

regularizePatches :: FL Patch C(x y) -> FL Patch C(x y)
regularizePatches patches = rpint (unsafeCoerceP NilFL) patches
    where -- this reverses the list, which seems odd and causes
          -- the witness unsafety
          rpint :: FL Patch C(x y) -> FL Patch C(a b) -> FL Patch C(x y)
          rpint ok_ps NilFL = ok_ps
          rpint ok_ps (p:>:ps) =
            if checkAPatch (unsafeCoerceP p:>:ok_ps)
            then rpint (unsafeCoerceP p:>:ok_ps) ps
            else rpint ok_ps ps

tryTokPossibly :: String -> String -> String
                -> (Maybe FileContents) -> (Maybe FileContents)
tryTokPossibly t o n = liftM $ \contents ->
        let lines' = M.mapMaybe (liftM B.concat
                                  . tryTokInternal t (BC.pack o) (BC.pack n))
                                (fcLines contents)
        in contents { fcLines = lines' }

tryTokInternal :: String -> B.ByteString -> B.ByteString
                 -> B.ByteString -> Maybe [B.ByteString]
tryTokInternal _ _ _ s | B.null s = Just []
tryTokInternal t o n s =
    case BC.break (regChars t) s of
    (before,s') ->
        case BC.break (not . regChars t) s' of
        (tok,after) ->
            case tryTokInternal t o n after of
            Nothing -> Nothing
            Just rest ->
                if tok == o
                then Just $ before : n : rest
                else if tok == n
                     then Nothing
                     else Just $ before : tok : rest

