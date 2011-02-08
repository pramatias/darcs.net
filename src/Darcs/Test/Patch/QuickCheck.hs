{-# OPTIONS_GHC -fno-warn-deprecations -fno-warn-orphans #-}
{-# LANGUAGE CPP, UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses,
             FlexibleInstances #-}

#include "gadts.h"
module Darcs.Test.Patch.QuickCheck ( WithStartState(..), RepoModel(..), Tree(..), TreeWithFlattenPos(..),
                                     propConsistentTreeFlattenings, propFail,
                                     propIsMergeable,
                                     flattenOne,
                                     commutePairFromTree, mergePairFromTree,
                                     commuteTripleFromTree, mergePairFromCommutePair,
                                     commutePairFromTWFP, mergePairFromTWFP, getPairs, getTriples,
                                     patchFromTree,
                                     canonizeTree,
                                     quickCheck, shrink
                                   ) where

import Control.Arrow ( (***) )
import Control.Monad ( liftM, replicateM, mplus, mzero )
import qualified Data.ByteString.Char8 as BC (pack, head)
import qualified Data.ByteString       as B  (ByteString, length)
import Test.QuickCheck
import Darcs.Witnesses.Sealed
import Darcs.Witnesses.Eq
import Darcs.Witnesses.Unsafe
import Darcs.Witnesses.Ordered
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Merge ( Merge(..) )
import Darcs.Patch.Patchy ( Invert(..), Commute(..) )
import Darcs.Patch.Prim ( PrimOf, PrimPatch, PrimPatchBase, FromPrim(..), move )
import Darcs.Patch.Prim.V1 ()
import Darcs.Patch.Prim.V1.Core ( Prim(..), FilePatchType(..), isIdentity )
import Darcs.Patch.V2 ( RealPatch, prim2real )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
--import Darcs.ColorPrinter ( errorDoc )
--import Darcs.ColorPrinter ( traceDoc )
import Darcs.Witnesses.Show
--import Printer ( greenText, ($$) )
import Darcs.Patch.FileName ( FileName, fp2fn )

#include "impossible.h"

data RepoModel C(x)
 = RepoModel {
     rmFileName :: !FileName,
     rmFileContents :: [B.ByteString]
   } deriving (Eq)

instance Show (RepoModel C(x)) where
  showsPrec d rm = showParen (d > appPrec) $
                   showString "RepoModel { rmFileName = " . showsPrec 0 (rmFileName rm) .
                   showString ", rmFileContents = " . showsPrec' 0 (rmFileContents rm) .
                   showString " }"
     where showsPrec' n ls | all ((==1) . B.length) ls = showsPrecC n ls
                           | otherwise = showsPrec n ls
           showsPrecC _ [] = showString "[]"
           showsPrecC n ss = showParen (n > 0) $ showString "packStringLetters " . showsPrec (appPrec + 1) (map BC.head ss)

instance Show1 RepoModel where
    showDict1 = ShowDictClass

-- | The initial repository model. The repository contains a single file named
--   @./file@, which is empty.
initRepoModel :: RepoModel C(Origin)
initRepoModel = RepoModel { rmFileName = fp2fn "./file", rmFileContents = [] }
rebuildRepoModel :: RepoModel C(x) -> RepoModel C(y)
rebuildRepoModel rm = RepoModel { rmFileName = rmFileName rm, rmFileContents = rmFileContents rm }

-- | A combination of a patch and its final state. The state, in this module, is
--   typically represented by a 'RepoModel' value. The @px@ type is typically a
--   patch type applied to its pre-state, e.g. @Prim x@.
data WithEndState px s C(y) = WithEndState { wesPatch :: px C(y), _wesState :: s C(y) }

-- | A type class to generate arbitrary values, threading a state through the
--   arbitrary calls. So this can be used to generate a patch that comes after
--   another patch. The post-state of the generated patch is hidden by the
--   'Sealed'.
class ArbitraryState s p where
  arbitraryState :: s C(x) -> Gen (Sealed (WithEndState (p C(x)) s))
  -- does a coarbitrary make sense?

-- | Generate a patch to a certain state.
class ArbitraryStateIn s p where
  arbitraryStateIn :: s C(x) -> Gen (p C(x))

instance ArbitraryState RepoModel Prim where
  arbitraryState rm = oneof [arbitraryFP rm]

instance Arbitrary (Sealed2 (FL Prim)) where
  arbitrary = do Sealed2 ps1 <- liftM (unseal (seal2 . wesPatch)) $ arbitraryState initRepoModel
                 return $ Sealed2 $ mapFL_FL make_identity_identity ps1
                     where make_identity_identity :: Prim C(x y) -> Prim C(x y)
                           make_identity_identity p | IsEq <- isIdentity p = localIdentity
                                                    | otherwise = p

instance Arbitrary (Sealed2 (FL (WithState RepoModel Prim))) where
  arbitrary = liftM (unseal (seal2 . wesPatch)) $ arbitraryState initRepoModel

propConsistentTreeFlattenings :: Sealed (WithStartState RepoModel (Tree Prim)) -> Bool
propConsistentTreeFlattenings (Sealed (WithStartState start t))
  = fromJust $
    do Sealed (G2 flat) <- return $ flattenTree $ mapTree prim2real t
       rms <- return $ map (applyPatch start) flat
       return $ and $ zipWith assertEqualFst (zip rms flat) (tail $ zip rms flat)

assertEqualFst :: (Eq a, Show a, Show b, Show c) => (a, b) -> (a, c) -> Bool
assertEqualFst (x,bx) (y,by)
    | x == y = True
    | otherwise = error ("Not really equal:\n" ++ show x ++ "\nand\n" ++ show y
                         ++ "\ncoming from\n" ++ show bx ++ "\nand\n" ++ show by)

-- WithState and propFail are handy for debugging arbitrary code
data WithState s p C(x y) = WithState (s C(x)) (p C(x y)) (s C(y))
  deriving Show

data WithStartState s p C(x) = WithStartState (s C(x)) (p C(x))

instance (Show1 s, Show1 p) => Show (WithStartState s p C(x)) where
   showsPrec d (WithStartState s p) = showParen (d > appPrec) $ showString "WithStartState " .
                                      showsPrec1 (appPrec + 1) s . showString " " .
                                      showsPrec1 (appPrec + 1) p

instance (Show1 s, Show1 p) => Show1 (WithStartState s p) where
   showDict1 = ShowDictClass

propFail :: Int -> Tree Prim C(x) -> Bool
propFail n xs = sizeTree xs < n

instance ArbitraryState s p => ArbitraryState s (WithState s p) where
  arbitraryState rm = do xandrm' <- arbitraryState rm
                         flip unseal xandrm' $ \(WithEndState x rm') ->
                           return $ seal $ WithEndState (WithState rm x rm') rm'

instance ArbitraryState s p => ArbitraryState s (FL p) where
  arbitraryState rm1 = sized $ \n -> do k <- choose (0, n)
                                        arbitraryList k rm1
      where arbitraryList :: FORALL(x) Int -> s C(x) -> Gen (Sealed (WithEndState (FL p C(x)) s))
            arbitraryList 0 rm = return $ seal $ WithEndState NilFL rm
            arbitraryList (n+1) rm = do Sealed (WithEndState x rm') <- arbitraryState rm
                                        Sealed (WithEndState xs rm'') <- arbitraryList n rm'
                                        return $ seal $ WithEndState (x :>: xs) rm''
            arbitraryList _ _ = impossible

data Tree p C(x) where
   NilTree :: Tree p C(x)
   SeqTree :: p C(x y) -> Tree p C(y) -> Tree p C(x)
   ParTree :: Tree p C(x) -> Tree p C(x) -> Tree p C(x)

mapTree :: (FORALL(y z) p C(y z) -> q C(y z)) -> Tree p C(x) -> Tree q C(x)
mapTree _ NilTree = NilTree
mapTree f (SeqTree p t) = SeqTree (f p) (mapTree f t)
mapTree f (ParTree t1 t2) = ParTree (mapTree f t1) (mapTree f t2)

instance Show2 p => Show (Tree p C(x)) where
   showsPrec _ NilTree = showString "NilTree"
   showsPrec d (SeqTree a t) = showParen (d > appPrec) $ showString "SeqTree " .
                               showsPrec2 (appPrec + 1) a . showString " " .
                               showsPrec (appPrec + 1) t
   showsPrec d (ParTree t1 t2) = showParen (d > appPrec) $ showString "ParTree " .
                                 showsPrec (appPrec + 1) t1 . showString " " .
                                 showsPrec (appPrec + 1) t2

instance Show2 p => Show1 (Tree p) where
    showDict1 = ShowDictClass

sizeTree :: Tree p C(x) -> Int
sizeTree NilTree = 0
sizeTree (SeqTree _ t) = 1 + sizeTree t
sizeTree (ParTree t1 t2) = 1 + sizeTree t1 + sizeTree t2

-- newtype G1 l p C(x) = G1 { _unG1 :: l (p C(x)) }
newtype G2 l p C(x y) = G2 { unG2 :: l (p C(x y)) }

flattenTree :: (Merge p) => Tree p C(z) -> Sealed (G2 [] (FL p) C(z))
flattenTree NilTree = seal $ G2 $ return NilFL
flattenTree (SeqTree p t) = mapSeal (G2 . map (p :>:) . unG2) $ flattenTree t
flattenTree (ParTree t1 t2) = flip unseal (flattenTree t1) $ \gpss1 ->
                              flip unseal (flattenTree t2) $ \gpss2 ->
                              seal $ G2 $
                              do ps1 <- unG2 gpss1
                                 ps2 <- unG2 gpss2
                                 ps2' :/\: ps1' <- return $ merge (ps1 :\/: ps2)
                                 -- We can't prove that the existential type in the result
                                 -- of merge will be the same for each pair of
                                 -- ps1 and ps2.
                                 map unsafeCoerceP [ps1 +>+ ps2', ps2 +>+ ps1']

instance ArbitraryState s p => ArbitraryStateIn s (Tree p) where
  -- don't generate trees deeper than 5 with default QuickCheck size
  arbitraryStateIn rm = sized $ \s -> arbitraryTree rm (s `div` 17)

-- | Generate a tree of patches, bounded by the depth @maxDepth@.
arbitraryTree :: ArbitraryState s p => s C(x) -> Int -> Gen (Tree p C(x))
arbitraryTree rm maxDepth
    | maxDepth == 0 = return NilTree
    | otherwise    = frequency [(2, return NilTree)
                               ,(2, do Sealed (WithEndState p rm') <- arbitraryState rm
                                       t <- arbitraryTree rm' maxDepth
                                       return (SeqTree p t))
                               ,(1, do t1 <- arbitraryTree rm (maxDepth - 1)
                                       t2 <- arbitraryTree rm (maxDepth - 1)
                                       return (ParTree t1 t2))]


shrinkWSSTree :: (Applyable prim, ShrinkablePos prim)
              => Sealed (WithStartState RepoModel (Tree prim))
              -> [Sealed (WithStartState RepoModel (Tree prim))]
shrinkWSSTree = unseal doShrinkWSSTree
 where
 doShrinkWSSTree wss@(WithStartState rm t)
  = shrinkWSSTree' wss -- shrink the tree
     `mplus`
    do -- shrink the starting context
      pos <- [0 .. length (rmFileContents rm) - 1]
      let rmFileContents' = take pos (rmFileContents rm) ++ drop (pos+1) (rmFileContents rm)
          t' = shrinkPosStart pos (canonizeTree t)
      return $ seal $ WithStartState (rm { rmFileContents = rmFileContents' }) (canonizeTree t')


-- If we had a more general repo model, then Int would need to be more general too

class ShrinkablePos p where
  shrinkPos :: Int -> p C(x y) -> (p C(x y), Maybe Int)
  shrinkPatch :: p C(x y) -> [(p C(x y), Maybe Int)]
  nullPatch :: p C(x y) -> EqCheck C(x y)

class ShrinkablePosStart p where
  shrinkPosStart :: Int -> p C(x) -> p C(x)

instance ShrinkablePos p => ShrinkablePosStart (Tree p) where
  shrinkPosStart _ NilTree = NilTree
  shrinkPosStart pos (SeqTree p t)
    = let (p', mpos') = shrinkPos pos p
      in case mpos' of
           Nothing -> SeqTree p' t
           Just pos' -> SeqTree p' (shrinkPosStart pos' t)
  shrinkPosStart pos (ParTree t1 t2) = ParTree (shrinkPosStart pos t1) (shrinkPosStart pos t2)

class (ArbitraryState RepoModel prim, ShrinkablePos prim, Applyable prim, PrimPatch prim) => ArbitraryPrim prim
instance ArbitraryPrim Prim

-- a hack introduced after Identity was removed from Prim
localIdentity :: PrimPatch prim => prim C(x x)
localIdentity = let fp = "./dummy" in move fp fp

smartFP :: FileName -> FilePatchType C(x y) -> Prim C(x y)
smartFP _ (Hunk _ [] []) | avoidEmptyHunks = unsafeCoerceP localIdentity
smartFP fn filepatch = FP fn filepatch

instance ShrinkablePos Prim where
  shrinkPos pos (FP fn fp) = (smartFP fn *** id) (shrinkPos pos fp)
  shrinkPos pos p | IsEq <- isIdentity p = (localIdentity, Just pos)
  shrinkPos _ _ = impossible
  shrinkPatch (FP fn fp) = map (smartFP fn *** id) (shrinkPatch fp)
  shrinkPatch p | IsEq <- isIdentity p = []
  shrinkPatch _ = impossible
  nullPatch (FP _ fp) = nullPatch fp
  nullPatch p | IsEq <- isIdentity p = IsEq
  nullPatch _ = impossible

avoidEmptyHunks :: Bool
avoidEmptyHunks = True

instance ShrinkablePos FilePatchType where
  shrinkPos pos (Hunk n old new)
    | pos < n-1 = (Hunk (n-1) old new, Just pos)
    | pos >= n-1 + length old = (Hunk n old new, Just (pos + length new - length old))
    | otherwise = (Hunk n (take pos' old ++ drop (pos'+1) old) new, Nothing)
        where pos' = pos - n + 1
  shrinkPos _ _ = bug "foo1 in ShrinkablePos"
  shrinkPatch (Hunk (n+1) [] []) | n > 0 = [(Hunk n [] [], Nothing)]
  shrinkPatch (Hunk n old new)
   = do i <- [0 .. length new - 1]
        return (Hunk n old (take i new ++ drop (i+1) new), Just (n + i))
  shrinkPatch _ = bug "foo in ShrinkablePos"
  nullPatch (Hunk _ [] []) = unsafeCoerceP IsEq -- is this safe?
  nullPatch _ = NotEq

shrinkWSSTree'
 -- start a new line here because apparently ' confuses CPP..
   :: (Applyable prim, ShrinkablePos prim)
   => WithStartState RepoModel (Tree prim) C(x)
   -> [Sealed (WithStartState RepoModel (Tree prim))]
shrinkWSSTree' (WithStartState _ NilTree) = []
shrinkWSSTree' (WithStartState rm t@(SeqTree p t'))
  = return (seal (WithStartState (applyPatch rm p) (canonizeTree t')))
     `mplus`
    liftM (seal . WithStartState rm) (map canonizeTree $ shrinkTree t)
shrinkWSSTree' (WithStartState rm t@(ParTree _ _))
  = liftM (seal . WithStartState rm) (map canonizeTree $ shrinkTree t)

-- canonize a tree, removing any dead branches
canonizeTree :: ShrinkablePos p => Tree p C(x) -> Tree p C(x)
canonizeTree NilTree = NilTree
canonizeTree (ParTree t1 t2)
    | NilTree <- canonizeTree t1 = canonizeTree t2
    | NilTree <- canonizeTree t2 = canonizeTree t1
    | otherwise = ParTree (canonizeTree t1) (canonizeTree t2)
canonizeTree (SeqTree p t) | IsEq <- nullPatch p = canonizeTree t
                           | otherwise = SeqTree p (canonizeTree t)

-- shrink the tree without changing the starting context
shrinkTree :: ShrinkablePos p => Tree p C(x) -> [Tree p C(x)]
shrinkTree NilTree = []
shrinkTree (SeqTree p t) = do case nullPatch p of
                                 IsEq -> return t
                                 NotEq -> mzero
                             `mplus`
                           do (p', pos) <- shrinkPatch p
                              return (SeqTree p' (maybe id shrinkPosStart pos t))
                             `mplus`
                           do t' <- shrinkTree t
                              return (SeqTree p t')
shrinkTree (ParTree t1 t2) = [t1, t2]
                              `mplus`
                             do t1' <- shrinkTree t1
                                return (ParTree t1' t2)
                              `mplus`
                             do t2' <- shrinkTree t2
                                return (ParTree t1 t2')

instance ArbitraryPrim prim => Arbitrary (Sealed (WithStartState RepoModel (Tree prim))) where
  arbitrary = do Sealed (WithStartState rm tree) <-
                     liftM (seal . WithStartState initRepoModel) (arbitraryStateIn initRepoModel)
                 return $ Sealed $ WithStartState rm (canonizeTree tree)
  shrink = shrinkWSSTree

propIsMergeable :: forall p C(x) . (FromPrim p, Merge p)
                  => Sealed (WithStartState RepoModel (Tree (PrimOf p)))
                  -> Maybe (Tree p C(x))
propIsMergeable (Sealed (WithStartState _ t))
   = case flattenOne t of
        Sealed ps -> let _ = seal2 ps :: Sealed2 (FL p)
                     in case lengthFL ps of
                       _ -> Nothing

flattenOne :: (FromPrim p, Merge p) => Tree (PrimOf p) C(x) -> Sealed (FL p C(x))
flattenOne NilTree = seal NilFL
flattenOne (SeqTree p t) = flip unseal (flattenOne t) $ \ps -> seal (fromPrim p :>: ps)
flattenOne (ParTree t1 t2) =
    flip unseal (flattenOne t1) $ \ps1 ->
    flip unseal (flattenOne t2) $ \ps2 ->
    --traceDoc (greenText "flattening two parallel series: ps1" $$ showPatch ps1 $$
    --          greenText "ps2" $$ showPatch ps2) $
    case merge (ps1 :\/: ps2) of
      ps2' :/\: _ -> seal (ps1 +>+ ps2')

instance ArbitraryPrim prim => Arbitrary (Sealed2 (FL (RealPatch prim))) where
    arbitrary = do Sealed (WithStartState _ tree) <- arbitrary :: Gen (Sealed (WithStartState RepoModel (Tree prim)))
                   return $ unseal seal2 (flattenOne tree)

instance ArbitraryPrim prim => Arbitrary (Sealed2 (RealPatch prim)) where
    arbitrary = do Sealed (WithStartState _ tree) <- arbitrary :: Gen (Sealed (WithStartState RepoModel (Tree prim)))
                   case mapFL seal2 `unseal` flattenOne tree of
                     [] -> return $ seal2 $ fromPrim localIdentity
                     ps -> elements ps

instance Arbitrary (Sealed2 (Prim :> Prim :> Prim)) where
    arbitrary = do Sealed2 ps <- arbitrary
                   return $ last_triple ps
        where last_triple :: FL Prim C(x y) -> Sealed2 (Prim :> Prim :> Prim)
              last_triple NilFL = seal2 $ localIdentity :> localIdentity :> localIdentity
              last_triple (a :>: NilFL) = seal2 $ a :> invert a :> localIdentity
              last_triple (a :>: b :>: NilFL) = seal2 $ invert a :> a :> b
              last_triple (a :>: b :>: c :>: NilFL) = seal2 $ a :> b :> c
              last_triple (_ :>: xs) = last_triple xs

data TreeWithFlattenPos p C(x) = TWFP Int (Tree p C(x))

commutePairFromTWFP :: (FromPrim p, Merge p, PrimPatchBase p)
                    => (FORALL (y z) (p :> p) C(y z) -> t)
                    -> (WithStartState RepoModel (TreeWithFlattenPos (PrimOf p)) C(x) -> t)
commutePairFromTWFP handlePair (WithStartState _ (TWFP n t))
    = unseal2 handlePair $
      let xs = unseal getPairs (flattenOne t)
      in if length xs > n && n >= 0 then xs!!n else seal2 (fromPrim localIdentity :> fromPrim localIdentity)

commutePairFromTree :: (FromPrim p, Merge p, PrimPatchBase p)
                    => (FORALL (y z) (p :> p) C(y z) -> t)
                    -> (WithStartState RepoModel (Tree (PrimOf p)) C(x) -> t)
commutePairFromTree handlePair (WithStartState _ t)
   = unseal2 handlePair $
     case flattenOne t of
       Sealed ps ->
         let xs = --traceDoc (greenText "I'm flattening one to get:" $$ showPatch ps) $
                 getPairs ps
         in if null xs then seal2 (fromPrim localIdentity :> fromPrim localIdentity) else last xs

commuteTripleFromTree :: (FromPrim p, Merge p, PrimPatchBase p)
                      => (FORALL (y z) (p :> p :> p) C(y z) -> t)
                      -> (WithStartState RepoModel (Tree (PrimOf p)) C(x) -> t)
commuteTripleFromTree handle (WithStartState _ t)
   = unseal2 handle $
     case flattenOne t of
       Sealed ps ->
         let xs = --traceDoc (greenText "I'm flattening one to get:" $$ showPatch ps) $
                  getTriples ps
         in if null xs
            then seal2 (fromPrim localIdentity :> fromPrim localIdentity :> fromPrim localIdentity)
            else last xs

mergePairFromCommutePair :: (Commute p, Invert p)
                         => (FORALL (y z) (p :\/: p) C(y z) -> t)
                         -> (FORALL (y z) (p :>   p) C(y z) -> t)
mergePairFromCommutePair handlePair (a :> b)
 = case commute (a :> b) of
     Just (b' :> _) -> handlePair (a :\/: b')
     Nothing -> handlePair (b :\/: b)

-- impredicativity problems mean we can't use (.) in the definitions below

mergePairFromTWFP :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
                  => (FORALL (y z) (p :\/: p) C(y z) -> t)
                  -> (WithStartState RepoModel (TreeWithFlattenPos (PrimOf p)) C(x) -> t)
mergePairFromTWFP x = commutePairFromTWFP (mergePairFromCommutePair x)

mergePairFromTree :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
                  => (FORALL (y z) (p :\/: p) C(y z) -> t)
                  -> (WithStartState RepoModel (Tree (PrimOf p)) C(x) -> t)
mergePairFromTree x = commutePairFromTree (mergePairFromCommutePair x)

patchFromCommutePair :: (Commute p, Invert p)
                     => (FORALL (y z) p C(y z) -> t)
                     -> (FORALL (y z) (p :> p) C(y z) -> t)
patchFromCommutePair handle (_ :> b) = handle b

patchFromTree :: (FromPrim p, Merge p, Invert p, PrimPatchBase p)
              => (FORALL (y z) p C(y z) -> t)
              -> (WithStartState RepoModel (Tree (PrimOf p)) C(x) -> t)
patchFromTree x = commutePairFromTree (patchFromCommutePair x)


instance Show2 p => Show (TreeWithFlattenPos p C(x)) where
   showsPrec d (TWFP n t) = showParen (d > appPrec) $ showString "TWFP " .
                            showsPrec (appPrec + 1) n . showString " " .
                            showsPrec1 (appPrec + 1) t

instance Show1 (TreeWithFlattenPos Prim) where
   showDict1 = ShowDictClass

instance Arbitrary (Sealed (WithStartState RepoModel (TreeWithFlattenPos Prim))) where
   arbitrary = do Sealed (WithStartState rm t) <- arbitrary
                  let num = unseal (length . getPairs) (flattenOneRP t)
                  if num == 0 then return $ Sealed $ WithStartState rm $ TWFP 0 NilTree
                    else do n <- choose (0, num - 1)
                            return $ Sealed $ WithStartState rm $ TWFP n t
                    where -- just used to get the length. In principle this should be independent of the patch type.
                          flattenOneRP :: Tree Prim C(x) -> Sealed (FL (RealPatch Prim) C(x))
                          flattenOneRP = flattenOne
   shrink (Sealed (WithStartState rm (TWFP n t))) =
      [Sealed (WithStartState rm' (TWFP n' t')) | Sealed (WithStartState rm' t') <- shrink (Sealed (WithStartState rm t)),
                                                  n' <- [0..n]]


getPairs :: FL p C(x y) -> [Sealed2 (p :> p)]
getPairs NilFL = []
getPairs (_:>:NilFL) = []
getPairs (a:>:b:>:c) = seal2 (a:>b) : getPairs (b:>:c)

getTriples :: FL p C(x y) -> [Sealed2 (p :> p :> p)]
getTriples NilFL = []
getTriples (_:>:NilFL) = []
getTriples (_:>:_:>:NilFL) = []
getTriples (a:>:b:>:c:>:d) = seal2 (a:>b:>c) : getTriples (b:>:c:>:d)

arbitraryFP :: RepoModel C(x) -> Gen (Sealed (WithEndState (Prim C(x)) RepoModel))
arbitraryFP rm
 = do (fp, newcontents) <- arbitraryFilePatchType (rmFileContents rm)
      return $ seal $ WithEndState (FP (rmFileName rm) fp) (rebuildRepoModel (rm { rmFileContents = newcontents }))

-- Really [B.ByteString] should be parametrised by x y, and the result tuple sealed on y
arbitraryFilePatchType :: [B.ByteString] -> Gen (FilePatchType C(x y), [B.ByteString])
arbitraryFilePatchType contents = oneof [arbitraryHunk contents]

arbitraryHunk :: [B.ByteString] -> Gen (FilePatchType C(x y), [B.ByteString])
arbitraryHunk contents
 = sized $ \n ->
   do start <- choose (0, min n (length contents))
      (removelen, _)
        <- frequency $ zip (if start == length contents then [1, 13, 0, 0] else [1, 3, 5, 3])
                [return (0, 0)
                ,do xa <- choose (0, n)
                    return (0, xa)
                ,do xr <- choose (1, min n (length contents - start))
                    return (xr, 0)
                ,do xr <- choose (1, min n (length contents - start))
                    xa <- choose (0, n)
                    return (xr, xa)
                ]
      addlen <- choose (0, n)
      additems <- replicateM addlen (do c <- choose ('a', 'z')
                                        return $ BC.pack [c])
      let newcontents = take start contents ++ additems ++ drop (start + removelen) contents
      return (Hunk (start+1) (take removelen $ drop start $ contents) additems, newcontents)

class Applyable p where
  applyPatch :: RepoModel C(x) -> p C(x y) -> RepoModel C(y)

instance Applyable p => Applyable (FL p) where
  applyPatch rm NilFL = rm
  applyPatch rm (p :>: ps) = applyPatch (applyPatch rm p) ps

instance (Applyable prim, PrimPatch prim) => Applyable (RealPatch prim) where
  applyPatch rm p = applyPatch rm (effect p)

instance Applyable Prim where
  applyPatch (rm@RepoModel { rmFileName = filename, rmFileContents = oldcontents }) (FP filename' fp)
     | filename == filename' = rebuildRepoModel (rm { rmFileContents = applyFilePatchType oldcontents fp })
  applyPatch rm p | IsEq <- isIdentity p = rm
  applyPatch _ _ = bug "We haven't defined applyPatch on Prim for all patch types."

applyFilePatchType :: [B.ByteString] -> FilePatchType C(x y) -> [B.ByteString]
applyFilePatchType oldcontents (Hunk n old new)
 = if take (length old) (drop (n - 1) oldcontents) == old
   then take (n - 1) oldcontents ++ new ++ drop (n - 1 + length old) oldcontents
   else error "Bad patch context"
applyFilePatchType _ _ = bug "We haven't defined applyFilePatchType for all patch types."
