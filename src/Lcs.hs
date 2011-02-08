-- Copyright (C) 2002 David Roundy
-- Copyright (C) 2005 Benedikt Schmidt
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

{-# LANGUAGE CPP #-}

-- | LCS stands for Longest Common Subsequence, and it is a relatively
-- challenging problem to find an LCS efficiently.  This module implements
-- the algorithm described in:
--
--   "An O(ND) Difference Algorithm and its Variations", Eugene Myers,
--   Algorithmica Vol. 1 No. 2, 1986, pp. 251-266;
--   especially the variation described in section 4.2 and most refinements
--   implemented in GNU diff (D is the edit-distance).
--
-- There is currently no heuristic to reduce the running time and produce
-- suboptimal output for large inputs with many differences. It behaves like
-- GNU diff with the -d option in this regard.
--
-- In the first step, a hash value for every line is calculated and collisions
-- are marked with a special value. This reduces a string comparison to an
-- int comparison for line tuples where at least one of the hash values is
-- not equal to the special value. After that, lines which only exists in one
-- of the files are removed and marked as changed which reduces the running
-- time of the following difference algorithm. GNU diff additionally removes
-- lines that appear very often in the other file in some cases.
-- The last step tries to create longer changed regions and line up deletions
-- in the first file to insertions in the second by shifting changed lines
-- forward and backward.
module Lcs ( getChanges,
             shiftBoundaries ) where

import Control.Monad
import Data.Int
import Control.Monad.ST
import Data.Maybe
import ByteStringUtils (hashPS)
import qualified Data.ByteString as B (empty, ByteString)
import Data.Array.Base
import Data.Array.Unboxed
import qualified Data.Map as Map ( lookup, empty, insertWith )
#include "impossible.h"

-- | create a list of changes between a and b, each change has the form
--   (starta, lima, startb, limb) which means that a[starta, lima)
--   has to be replaced by b[startb, limb)
getChanges ::  [B.ByteString] -> [B.ByteString]
           -> [(Int,[B.ByteString],[B.ByteString])]
getChanges a b = dropStart (initP a) (initP b) 1

dropStart ::  PArray -> PArray -> Int
           -> [(Int,[B.ByteString],[B.ByteString])]
dropStart a b off
  | off > aLen a = [(off - 1, [], getSlice b off (aLen b))]
  | off > aLen b = [(off - 1, getSlice a off (aLen a), [])]
  | a!off == b!off = dropStart a b (off + 1)
  | otherwise      = dropEnd a b off 0

dropEnd ::  PArray -> PArray -> Int -> Int
        -> [(Int,[B.ByteString],[B.ByteString])]
dropEnd a b off end
    | off > alast        = [(off - 1, [], getSlice b off blast)]
    | off > blast        = [(off - 1, getSlice a off alast, [])]
    | a!alast == b!blast = dropEnd a b off (end + 1)
    | otherwise          = getChanges' (a, (off, alast)) (b, (off, blast))
  where alast = aLen a - end
        blast = aLen b - end

getSlice :: PArray -> Int -> Int -> [B.ByteString]
getSlice a from to
  | from > to = []
  | otherwise = (a ! from) : (getSlice a (from + 1) to)

getChanges' :: (PArray, (Int, Int)) -> (PArray, (Int, Int))
            -> [(Int,[B.ByteString],[B.ByteString])]
getChanges' (a, abounds) (b, bbounds) =
    map (convertPatch 0 a b) $ createPatch c_a c_b
  where
        -- If the last few characters of two lines are the same, the lines are
        -- probably the same. The choice of 20 is plucked out of the air.
        toHash x bnds = listArray bnds [ hashPS $ x!i | i <- range bnds]
        ah = toHash a abounds :: HArray
        mkAMap m (i:is) =
            let ins (_,_,_,new) (collision,_,_,old) =
                    (collision || (new /= old), True, False, old)
                m' = Map.insertWith ins (ah!i) (False, True, False, a!i) m
            in mkAMap m' is
        mkAMap m _ = m
        hm_a = mkAMap Map.empty (range abounds)
        --
        bh = toHash b bbounds :: HArray
        mkBMap m (i:is) =
            let ins (_,_,_,new) (collision,in_a,_,old) =
                    (collision || (new /= old), in_a, True, old)
                m' = Map.insertWith ins (bh!i) (False, False, True, b!i) m
            in mkBMap m' is
        mkBMap m _ = m
        hm = mkBMap hm_a (range bbounds)
        -- take care of collisions, if there are different lines with the
        -- same hash in both files, then set the hash to markColl,
        -- PackedStrings are compared for two lines with the hash markColl
        get (i, h) = case Map.lookup h hm of
                      Just (_,False,_,_) -> Nothing
                      Just (_,_,False,_) -> Nothing
                      Just (False,True,True,_) -> Just (i, h)
                      Just (True,True,True,_) -> Just (i, markColl)
                      Nothing -> impossible

        a' = mapMaybe get [(i, ah!i) | i <- range (bounds ah)]
        b' = mapMaybe get [(i, bh!i) | i <- range (bounds bh)]

        (c_a, c_b) = diffArr a' b' (a, abounds) (b, bbounds)

-- | mark hash value where collision occured
markColl :: Int32
markColl = 2345677

-- | return arrays with changes in a and b (1 indexed), offsets start with 0
diffArr :: [(Int,Int32)] -> [(Int,Int32)]
        -> (PArray, (Int, Int)) -> (PArray, (Int, Int))
        -> (BArray, BArray)
diffArr a b (p_a, (off_a, l_a)) (p_b, (off_b, l_b)) = runST (
  do let h_a = initH (map snd a)
         h_b = initH (map snd b)
         m_a = initM (map fst a)
         m_b = initM (map fst b)
         end_a = (aLen p_a)
         end_b = (aLen p_b)
     c_a <- initVChanged end_a
     c_b <- initVChanged end_b
     mapM_ (\ (l,_) -> writeArray c_a l False) a
     mapM_ (\ (l,_) -> writeArray c_b l False) b
     _ <- cmpseq h_a h_b p_a p_b m_a m_b c_a c_b 0 0 (aLen h_a) (aLen h_b)
     let unchanged ar = do {xs <- getElems ar; return $ length (filter not xs) -1}
     err <- liftM2 (/=) (unchanged c_a) (unchanged c_b)
     when err impossible
     -- Mark common lines at beginning and end
     mapM_ (\ i -> writeArray c_a i False ) [1..(off_a - 1)]
     mapM_ (\ i -> writeArray c_b i False ) [1..(off_b - 1)]
     mapM_ (\ i -> writeArray c_a i False ) [(l_a + 1) .. end_a]
     mapM_ (\ i -> writeArray c_b i False ) [(l_b + 1) .. end_b]
     shiftBoundaries c_a c_b p_a 1 1
     shiftBoundaries c_b c_a p_b 1 1
     err1 <- liftM2 (/=) (unchanged c_a) (unchanged c_b)
     when err1 impossible
     c_a' <- unsafeFreeze c_a
     c_b' <- unsafeFreeze c_b
     return (c_a', c_b'))

-- | set changes array for a and b and return number of changed lines
cmpseq :: HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
       -> BSTArray s -> BSTArray s -> Int -> Int -> Int -> Int -> ST s Int
cmpseq _ _ _ _ _ _ _ _ _ _ 0 0 = return 0
cmpseq h_a h_b p_a p_b m_a m_b c_a c_b off_a off_b l_a l_b = do
  let lim_a = off_a+l_a
      lim_b = off_b+l_b
      off_a' = findSnake h_a h_b p_a p_b m_a m_b off_a off_b l_a l_b off_a off_b
      off_b' = off_b+off_a'-off_a
      lim_a' = findSnakeRev h_a h_b p_a p_b m_a m_b lim_a lim_b off_a' off_b'
      lim_b' = lim_b+lim_a'-lim_a
      l_a' = lim_a'-off_a'
      l_b' = lim_b'-off_b'
  if l_a' == 0 || l_b' == 0
     then if l_a' == 0
             then do when (l_b' > 0) $
                          mapM_ (\i -> writeArray c_b (m_b!i) True)
                                [(off_b' + 1) .. lim_b']
                     return l_b'
             else do when (l_a' > 0) $
                          mapM_ (\i -> writeArray c_a (m_a!i) True)
                                [(off_a' + 1) .. lim_a']
                     return l_a'
     else do let m = l_a' + l_b'
                 del = l_a' - l_b'
                 dodd = odd del
             v <- initV m
             vrev <- initVRev m l_a'
             writeArray vrev 0 l_a'
             writeArray v 0 0
             (xmid, ymid, _) <- findDiag 1 h_a h_b p_a p_b m_a m_b v vrev
                                off_a' off_b' l_a' l_b' del dodd
             when ((xmid == 0 && ymid == 0) || (xmid == l_a' && ymid == l_b')
                   || (xmid < 0 || ymid < 0 || xmid > l_a' || ymid > l_b'))
                     impossible
             c1 <- cmpseq h_a h_b p_a p_b m_a m_b c_a c_b
                          off_a' off_b' xmid ymid
             c2 <- cmpseq h_a h_b p_a p_b m_a m_b c_a c_b
                          (off_a' + xmid) (off_b' + ymid)
                          (l_a' - xmid) (l_b' - ymid)
             return $ c1 + c2

-- | return (xmid, ymid, cost) for the two substrings
--   a[off_a+1..off_a+1+l_a] and b
findDiag :: Int -> HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
         -> VSTArray s -> VSTArray s -> Int -> Int -> Int -> Int -> Int -> Bool
         -> ST s (Int, Int, Int)
findDiag c h_a h_b p_a p_b m_a m_b v vrev off_a off_b l_a l_b del dodd = do
  when (c > l_a + l_b) $ error "findDiag failed"
  r <- findF
  case r of
    Just (xmid, ymid) -> return (xmid, ymid, (c*2 - 1))
    Nothing ->
      do r' <- findR
         case r' of
           Just (xmid, ymid) -> return (xmid, ymid, c*2)
           Nothing -> findDiag (c + 1) h_a h_b p_a p_b m_a m_b v vrev
                      off_a off_b l_a l_b del dodd
 where fdmax = if c <= l_a then c else l_a - ((l_a + c) `mod` 2)
       rdmax = if c <= l_b then c else l_b - ((l_b + c) `mod` 2)
       lastrdmax = if (c-1) <= l_b then c-1 else l_b-(l_b + (c-1) `mod` 2)
       lastrdmin = -(if (c-1) <= l_a then c-1 else l_a-((l_a + (c-1)) `mod` 2))
       fdmin = -rdmax
       rdmin = -fdmax
       findF = findF' fdmax
       findR = findR' rdmax
       findF' d = do x <- findOne h_a h_b p_a p_b m_a m_b v d off_a off_b l_a l_b
                     if dodd && d - del >= lastrdmin && d - del <= lastrdmax
                        then do xr <- readArray vrev (d - del)
                                if xr <= x then return $ Just (x, x - d)
                                           else if d <= fdmin then return Nothing
                                                              else findF' (d-2)
                        else if d <= fdmin then return Nothing else findF' (d-2)
       findR' d = do x <- findOneRev h_a h_b p_a p_b m_a m_b vrev d del off_a off_b
                     if not dodd && (d + del >= fdmin) && (d + del <= fdmax)
                        then do xf <- readArray v (d + del)
                                if x <= xf then return $ Just (x,x-del-d)
                                           else if d <= rdmin then return Nothing
                                                              else findR' (d-2)
                        else if d <= rdmin then return Nothing else findR' (d-2)

-- | find position on diag d with one more insert/delete going forward
findOne  :: HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
         -> VSTArray s -> Int -> Int -> Int -> Int -> Int -> ST s Int
findOne h_a h_b p_a p_b m_a m_b v d off_a off_b l_a l_b = do
  x0 <- do xbelow <- readArray v (d - 1)
           xover <- readArray v (d + 1)
           return $ if xover > xbelow then xover else xbelow + 1
  let y0 = x0 - d
      x = findSnake h_a h_b p_a p_b  m_a m_b (x0+off_a) (y0+off_b)
            l_a l_b off_a off_b
  writeArray v d (x - off_a)
  return (x-off_a)

-- | follow snake from northwest to southeast, x and y are absolute positions
findSnake :: HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
          -> Int -> Int -> Int -> Int -> Int -> Int -> Int
findSnake h_a h_b p_a p_b  m_a m_b x y l_a l_b off_a off_b =
  if x < l_a + off_a && y < l_b + off_b && h_a!(x+1) == h_b!(y+1)
       && (h_a!(x+1) /= markColl || p_a!(m_a!(x+1)) == p_b!(m_b!(y+1)))
     then findSnake h_a h_b p_a p_b m_a m_b (x + 1) (y + 1) l_a l_b off_a off_b
     else x

-- | find position on diag d with one more insert/delete going backward
findOneRev :: HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
           -> VSTArray s -> Int -> Int -> Int -> Int -> ST s Int
findOneRev h_a h_b p_a p_b m_a m_b v d del off_a off_b = do
  x0 <- do xbelow <- readArray v (d - 1)
           xover <- readArray v (d + 1)
           return $ if xbelow < xover then xbelow else xover-1
  let y0 = x0 - del - d
      x = findSnakeRev h_a h_b p_a p_b m_a m_b (x0+off_a) (y0+off_b)
            off_a off_b
  writeArray v d (x-off_a)
  return (x-off_a)

-- | follow snake from southeast to northwest, x and y are absolute positions
findSnakeRev :: HArray -> HArray -> PArray -> PArray -> MapArray -> MapArray
             -> Int -> Int -> Int -> Int -> Int
findSnakeRev h_a h_b p_a p_b m_a m_b x y off_a off_b =
  if x > off_a && y > off_b && h_a!x == h_b!y
       && (h_a!x /= markColl || p_a!(m_a!x) == p_b!(m_b!y))
     then findSnakeRev h_a h_b p_a p_b m_a m_b (x - 1) (y - 1) off_a off_b
     else x

-- | try to create nicer diffs by shifting around regions of changed lines
shiftBoundaries :: BSTArray s -> BSTArray s -> PArray -> Int -> Int -> ST s ()
shiftBoundaries c_a c_b p_a i_ j_ =
  do x <- nextChanged c_a i_
     case x of
       Just start ->
             do let skipped = start - i_
                j1 <- nextUnchangedN c_b skipped j_
                end <- nextUnchanged c_a start
                j2 <- nextUnchanged c_b j1
                (i3,j3) <- expand start end j2
                shiftBoundaries c_a c_b p_a i3 j3
       Nothing -> return () -- no change up to end of file
 where noline = (aLen p_a) + 1
       expand start i j =
         do let len = i - start
            (start0,i0,j0) <- shiftBackward start i j
            b <- if j0 > 1 then readArray c_b (j0-1) else return False
            let corr = if b then i0 else noline
            let blank = if p_a!(i0-1) == B.empty then i0
                                               else noline
            (start1,i1,j1,corr1,blank1) <- shiftForward start0 i0 j0 corr blank
            -- prefer corresponding to ending with blank line
            let newi = if corr1 == noline then blank1
                                          else corr1
            (start2,i2,j2) <- moveCorr start1 i1 j1 newi
            if len /= i2 - start2
                then expand start2 i2 j2
                else return (i2, j2)
       shiftBackward start i j =
         if start > 1 && p_a!(i-1) == p_a!(start-1)
            then do when (i == start) impossible
                    b1 <- readArray c_a (i-1)
                    b2 <- readArray c_a (start-1)
                    when (not b1 || b2) impossible
                    writeArray c_a (i-1) False
                    writeArray c_a (start-1) True
                    b <- if start > 2 then readArray c_a (start-2)
                                      else return False
                    start' <- if b then liftM (1+) (prevUnchanged c_a (start-2))
                                   else return (start-1)
                    j' <- prevUnchanged c_b (j-1)
                    shiftBackward start' (i-1) j'
            else return (start,i,j)
       shiftForward start i j corr blank =
         if i <= aLen p_a && p_a!i == p_a!start &&
             -- B.empty at the end of file marks empty line after final newline
             not ((i == aLen p_a) && (p_a!i == B.empty))
            then do when (i == start) impossible
                    b1 <- readArray c_a i
                    b2 <- readArray c_a start
                    when (not b2 ||  b1) impossible
                    writeArray c_a i True
                    writeArray c_a start False
                    i0 <- nextUnchanged c_a (i+1)
                    j0 <- nextUnchanged c_b (j+1)
                    let corr0 = if i0 > (i+1) then noline
                                              else if j0-j > 2 then i0 else corr
                    let blank0 = if i0 > i+1 then noline
                                 else if p_a!(i0-1) == B.empty then i0
                                                             else blank
                    shiftForward (start+1) i0 j0 corr0 blank0
            else return (start,i,j,corr,blank)
       moveCorr start i j corr =
         if corr >= i
            then return (start,i,j)
            else do b1 <- readArray c_a (i-1)
                    b2 <- readArray c_a (start-1)
                    when (not b1 || b2) impossible
                    when (p_a!(i-1) /= p_a!(start-1)) impossible
                    writeArray c_a (i-1) False
                    writeArray c_a (start-1) True
                    j' <- prevUnchanged c_b (j-1)
                    moveCorr (start-1) (i-1) j' corr

-- | goto next unchanged line, return the given line if unchanged
nextUnchanged :: BSTArray s -> Int -> ST s Int
nextUnchanged c i = do
  len <- aLenM c
  if i == len + 1 then return i
     else do b <- readArray c i
             if b then nextUnchanged c (i+1)
                  else return i

-- | skip at least one unchanged line, if there is none advance
--   behind the last line
skipOneUnChanged :: BSTArray s -> Int -> ST s Int
skipOneUnChanged c i = do
  len <- aLenM c
  if i == len + 1
     then return i
     else do b <- readArray c i
             if not b then return (i+1)
                      else skipOneUnChanged c (i+1)

-- | goto n-th next unchanged line
nextUnchangedN :: BSTArray s -> Int -> Int -> ST s Int
nextUnchangedN c n i =
  if n == 0 then return i
            else do i' <- skipOneUnChanged c i
                    nextUnchangedN c (n-1) i'

-- | goto next changed line, return the given line if changed
nextChanged :: BSTArray s -> Int -> ST s (Maybe Int)
nextChanged c i = do
  len <- aLenM c
  if i <= len
    then do b <- readArray c i
            if not b then nextChanged c (i+1)
                     else return $ Just i
    else return Nothing

-- | goto previous unchanged line, return the given line if unchanged
prevUnchanged :: BSTArray s -> Int -> ST s Int
prevUnchanged c i = do
  b <- readArray c i
  if b then prevUnchanged c (i-1)
       else return i

type HArray = UArray Int Int32
type BArray = UArray Int Bool
type PArray = Array Int B.ByteString
type MapArray = UArray Int Int
type VSTArray s = STUArray s Int Int
type BSTArray s = STUArray s Int Bool

initV :: Int -> ST s (VSTArray s)
initV dmax = newArray (-(dmax + 1), dmax + 1) (-1)

initVRev :: Int -> Int -> ST s (VSTArray s)
initVRev dmax xmax = newArray (-(dmax + 1), dmax + 1) (xmax + 1)

-- 1 indexed, v[0] is used as a guard element
initVChanged :: Int -> ST s (BSTArray s)
initVChanged l = do
  a <- newArray (0, l) True
  writeArray a 0 False
  return a
  -- set to false for all lines which have a mapping later
  -- other lines are only present in one of the files

initH :: [Int32] -> HArray
initH a = listArray (0, length a) (0:a)

initM :: [Int] -> MapArray
initM a = listArray (0, length a) (0:a)

initP :: [B.ByteString] -> PArray
initP a = listArray (0, length a) (B.empty:a)

aLen :: (IArray a e) => a Int e -> Int
aLen a = snd $ bounds a
aLenM :: (MArray a e m) => a Int e -> m Int
aLenM a = getBounds a >>= return . snd

convertPatch :: Int -> PArray -> PArray -> (Int, Int, Int, Int)
             -> (Int,[B.ByteString],[B.ByteString])
convertPatch off a b (a0,a1,b0,b1)
 | b0 == b1 = (b0+off,getDelete a a0 a1,[])
 | a0 == a1 = (b0+off,[],getInsert b b0 b1)
 | otherwise = (b0+off,getDelete a a0 a1,getInsert b b0 b1)

getInsert :: PArray -> Int -> Int -> [B.ByteString]
getInsert b from to
  | from >= to = []
  | otherwise = (b!(from+1)):(getInsert b (from+1) to)
getDelete :: PArray -> Int -> Int -> [B.ByteString]
getDelete a from to
  | from >= to = []
  | otherwise = (a!(from+1)):(getDelete a (from+1) to)

createPatch :: BArray -> BArray -> [(Int, Int, Int, Int)]
createPatch c_a c_b =
  reverse $ createP c_a c_b (aLen c_a) (aLen c_b)

createP :: BArray -> BArray -> Int -> Int -> [(Int, Int, Int, Int)]
createP _ _ 0 0 = []
createP c_a c_b ia ib =
  if c_a!ia || c_b!ib
     then let ia' = skipChangedRev c_a ia
              ib' = skipChangedRev c_b ib
          in (ia',ia,ib',ib):(createP c_a c_b ia' ib')
     else createP c_a c_b (ia-1) (ib-1)

skipChangedRev :: BArray -> Int -> Int
skipChangedRev c i = if i >= 0 && c!i then skipChangedRev c (i-1) else i
