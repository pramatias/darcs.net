{-# LANGUAGE CPP, GADTs #-}

#include "gadts.h"
module Darcs.ProgressPatches (progressRL, progressFL, progressRLShowTags)
where
import Darcs.Witnesses.Ordered ( FL(..), RL(..), lengthRL, lengthFL )
import Darcs.Patch.PatchInfoAnd (PatchInfoAnd,info)
import System.IO.Unsafe ( unsafePerformIO )
import Progress (minlist, beginTedious,
                 endTedious, progress, progressKeepLatest,
                 tediousSize, finishedOne)
import Darcs.Patch.Info (justName, isTag)


-- | Evaluate an 'FL' list and report progress.
progressFL :: String -> FL a C(x y) -> FL a C(x y)
progressFL _ NilFL = NilFL
progressFL k (x:>:xs) = if l < minlist then x:>:xs
                                       else startit x :>: pl xs
    where l = lengthFL (x:>:xs)
          startit y = unsafePerformIO $ do beginTedious k
                                           tediousSize k l
                                           return y
          pl :: FL a C(x y) -> FL a C(x y)
          pl NilFL = NilFL
          pl (y:>:NilFL) = unsafePerformIO $ do endTedious k
                                                return (y:>:NilFL)
          pl (y:>:ys) = progress k y :>: pl ys

-- | Evaluate an 'RL' list and report progress.
progressRL :: String -> RL a C(x y) -> RL a C(x y)
progressRL _ NilRL = NilRL
progressRL k (x:<:xs) = if l < minlist then x:<:xs
                                       else startit x :<: pl xs
    where l = lengthRL (x:<:xs)
          startit y = unsafePerformIO $ do beginTedious k
                                           tediousSize k l
                                           return y
          pl :: RL a C(x y) -> RL a C(x y)
          pl NilRL = NilRL
          pl (y:<:NilRL) = unsafePerformIO $ do endTedious k
                                                return (y:<:NilRL)
          pl (y:<:ys) = progress k y :<: pl ys

-- | Evaluate an 'RL' list and report progress. In addition to printing
-- the number of patches we got, show the name of the last tag we got.
progressRLShowTags :: String -> RL (PatchInfoAnd p) C(x y)
                   -> RL (PatchInfoAnd p) C(x y)
progressRLShowTags _ NilRL = NilRL
progressRLShowTags k (x:<:xs) = if l < minlist then x:<:xs
                                       else startit x :<: pl xs
    where l = lengthRL (x:<:xs)
          startit y = unsafePerformIO $ do beginTedious k
                                           tediousSize k l
                                           return y
          pl :: RL (PatchInfoAnd p) C(x y) -> RL (PatchInfoAnd p) C(x y)
          pl NilRL = NilRL
          pl (y:<:NilRL) = unsafePerformIO $ do endTedious k
                                                return (y:<:NilRL)
          pl (y:<:ys) =
              if isTag iy
              then finishedOne k ("back to "++ justName iy) y :<: pl ys
              else progressKeepLatest k y :<: pl ys
                  where
                    iy = info y
