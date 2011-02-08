-- Copyright (C) 2007 David Roundy
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
{-# LANGUAGE CPP, FlexibleContexts, UndecidableInstances #-}

#include "gadts.h"

-- |'NonPatch' and 'Non' patches are patches that store a context as a
-- sequence of patches.  See "Darcs.Patch.Real" for example usage.
module Darcs.Patch.V2.Non
       ( Non(..), Nonable(..), unNon,
         showNon, readNon, showNons, readNons,
         add, addP, remP, addPs, remPs, remNons,
         (*>), (>*), (*>>), (>>*),
         propAdjustTwice ) where

import Prelude hiding ( rem )
import Data.List ( delete )
import Control.Monad ( liftM, mzero )
import Darcs.Patch.Commute ( commuteFLorComplain )
import Darcs.Patch.Effect ( Effect(..) )
import Darcs.Patch.Format ( PatchListFormat, FileNameFormat(..) )
import Darcs.Patch.Invert ( Invert, invertFL, invertRL )
import Darcs.Patch.Prim ( FromPrim(..), ToFromPrim(..),
                          PrimOf, PrimPatchBase,
                          showPrim, sortCoalesceFL,
                          readPrim )
import Darcs.Patch.Patchy ( Patchy, showPatch, ReadPatch(..),
                            Commute(..), invert )
import Darcs.Patch.ReadMonads ( ParserM, lexChar )
import Darcs.Witnesses.Eq ( MyEq(..), EqCheck(..) )
import Darcs.Witnesses.Ordered ( FL(..), RL(..), (+>+), mapRL_RL
                               , (:>)(..), reverseFL, reverseRL )
import Darcs.Patch.Read ( peekfor )
import Darcs.Patch.Show ( ShowPatchBasic )
import Darcs.Patch.Viewing ()
import Darcs.Patch.Permutations ( removeFL, commuteWhatWeCanFL )
import Darcs.Witnesses.Show ( ShowDict(..), Show1(..), Show2(..), appPrec
                            , showsPrec2 )
import Darcs.Witnesses.Sealed ( Sealed(Sealed) )
import Printer ( Doc, empty, vcat, hiddenPrefix, blueText, redText, ($$) )
import qualified Data.ByteString.Char8 as BC ( pack, singleton, ByteString )

--import Darcs.ColorPrinter ( traceDoc )
--import Printer ( greenText )

showNons :: (ShowPatchBasic p, PatchListFormat p, PrimPatchBase p) => [Non p C(x)] -> Doc
showNons [] = empty
showNons xs = blueText "{{" $$ vcat (map showNon xs) $$ blueText "}}"

oBracket :: BC.ByteString
oBracket = BC.pack "{{"

cBracket :: BC.ByteString
cBracket = BC.pack "}}"

showNon :: (ShowPatchBasic p, PatchListFormat p, PrimPatchBase p) => Non p C(x) -> Doc
showNon (Non c p) = hiddenPrefix "|" (showPatch c)
                    $$ hiddenPrefix "|" (blueText ":")
                    $$ showPrim NewFormat p

readNons :: (ReadPatch p, PatchListFormat p, PrimPatchBase p, ParserM m) => m [Non p C(x)]
readNons = peekfor oBracket rns (return [])
    where rns = peekfor cBracket (return []) $
                do Sealed ps <- readPatch'
                   lexChar ':'
                   Sealed p <- readPrim NewFormat
                   (Non ps p :) `liftM` rns

readNon :: (ReadPatch p, PatchListFormat p, PrimPatchBase p, ParserM m) => m (Non p C(x))
readNon = do Sealed ps <- readPatch'
             peekfor colon (do Sealed p <- readPrim NewFormat
                               return $ Non ps p)
                           mzero

colon :: BC.ByteString
colon = BC.singleton ':'

instance (Commute p, MyEq p, MyEq (PrimOf p)) => Eq (Non p C(x)) where
    Non (cx :: FL p C(x y1)) (x :: PrimOf p C(y1 z1))
     == Non (cy :: FL p C(x y2)) (y :: PrimOf p C(y2 z2)) =
      case cx =\/= cy of
        IsEq -> case x =\/= y :: EqCheck C(z1 z2) of
                  IsEq -> True
                  NotEq -> False
        NotEq -> False

-- | 'Non' stores a context with a 'Prim' patch.
data Non p C(x) where
    Non :: FL p C(a x) -> PrimOf p C(x y) -> Non p C(a)

-- | Return as a list the context followed by the primitive patch.
unNon :: FromPrim p => Non p C(x) -> Sealed (FL p C(x))
unNon (Non c x) = Sealed (c +>+ fromPrim x :>: NilFL)

class Nonable p where
    non :: p C(x y) -> Non p C(x)

-- | 'addP' @x cy@ tries to commute @x@ past @cy@ and always returns some
-- variant @cy'@.  -- commutation suceeds, the variant is just
-- straightforwardly the commuted versian.  If commutation fails, the variant
-- consists of @x@ prepended to the context of @cy@.
addP :: (Patchy p, ToFromPrim p) => p C(x y) -> Non p C(y) -> Non p C(x)
addP p n | Just n' <- p >* n = n'
addP p (Non c x) = Non (p:>:c) x

-- | 'addPs' @xs cy@ commutes as many patches of @xs@ past @cy@ as
--   possible, stopping at the first patch that fails to commute.
--   Note the fact @xs@ is a 'RL'
--
--   Suppose we have
--
--   > x1 x2 x3 [c1 c2 y]
--
--   and that in our example @c1@ fails to commute past @x1@, this
--   function would commute down to
--
--   > x1 [c1'' c2'' y''] x2' x3'
--
--   and return @[x1 c1'' c2'' y'']@
addPs :: (Patchy p, ToFromPrim p) => RL p C(x y) -> Non p C(y) -> Non p C(x)
addPs NilRL n = n
addPs (p:<:ps) n = addPs ps $ addP p n

add :: (WL l, Patchy p, ToFromPrim p) => l (PrimOf p) C(x y) -> Non p C(y) -> Non p C(x)
add q = addPs (mapRL_RL fromPrim $ toRL q)

-- remNons really only works right if the relevant nons are conflicting...
remNons :: (Nonable p, Effect p, Patchy p, ToFromPrim p, PrimPatchBase p, MyEq (PrimOf p)) => [Non p C(x)] -> Non p C(x) -> Non p C(x)
remNons ns (Non c x) = case remNonHelper ns c of
                       NilFL :> c' -> Non c' x
                       _ -> Non c x

-- |abstract over 'FL'/'RL'
class WL l where
   toFL :: l p C(x y) -> FL p C(x y)
   toRL :: l p C(x y) -> RL p C(x y)
   invertWL :: Invert p => l p C(x y) -> l p C(y x)

instance WL FL where
   toFL = id
   toRL = reverseFL
   invertWL = reverseRL . invertFL

instance WL RL where
   toFL = reverseRL
   toRL = id
   invertWL = reverseFL . invertRL

remNonHelper :: (Nonable p, Effect p, Patchy p, ToFromPrim p, PrimPatchBase p, MyEq (PrimOf p)) => [Non p C(x)] -> FL p C(x y)
             -> (FL (PrimOf p) :> FL p) C(x y)
remNonHelper [] x = NilFL :> x
remNonHelper ns (c:>:cs)
    | non c `elem` ns = case remNonHelper (map (addP $ invert c) $ delete (non c) ns) cs of
                        a :> z -> sortCoalesceFL (effect c+>+a) :> z
    | otherwise = case commuteWhatWeCanFL (c :> cs) of
                  b :> c' :> d ->
                      case remNonHelper ns b of
                      a :> b' -> a :> (b'+>+c':>:d)
remNonHelper _ NilFL = NilFL :> NilFL

remP :: (Patchy p, ToFromPrim p) => p C(x y) -> Non p C(x) -> Maybe (Non p C(y))
remP p n | Just n' <- n *> p = Just n'
remP p (Non pc x) = do c <- removeFL p pc
                       return (Non c x)

remPs :: (Patchy p, ToFromPrim p) => FL p C(x y) -> Non p C(x) -> Maybe (Non p C(y))
remPs NilFL n = Just n
remPs (p:>:ps) n = remP p n >>= remPs ps

(*>) :: (Patchy p, ToFromPrim p) => Non p C(x) -> p C(x y) -> Maybe (Non p C(y))
n *> p = invert p >* n

(>*) :: (Patchy p, ToFromPrim p) => p C(x y) -> Non p C(y) -> Maybe (Non p C(x))
y >* (Non c x) = case commuteFLorComplain (y :> c) of
                    Right (c' :> y') -> do
                      px' :> _ <- commute (y' :> fromPrim x)
                      x' <- toPrim px'
                      return (Non c' x')
                    _ -> Nothing

(*>>) :: (WL l, Patchy p, ToFromPrim p, PrimPatchBase p) => Non p C(x) -> l (PrimOf p) C(x y) -> Maybe (Non p C(y))
n *>> p = invertWL p >>* n

(>>*) :: (WL l, Patchy p, ToFromPrim p) => l (PrimOf p) C(x y) -> Non p C(y) -> Maybe (Non p C(x))
q >>* nn = adj (toRL q) nn
    where adj :: (Patchy p, ToFromPrim p) => RL (PrimOf p) C(x y) -> Non p C(y) -> Maybe (Non p C(x))
          adj NilRL n = Just n
          adj (x:<:xs) n = fromPrim x >* n >>= adj xs

-- TODO why don't any tests run this?
propAdjustTwice :: (Patchy p, ToFromPrim p, MyEq (PrimOf p)) => p C(x y) -> Non p C(y) -> Maybe Doc
propAdjustTwice p n =
    do n' <- p >* n
       case n' *> p of
         Nothing -> Just (redText "prop_adjust_inverse 1")
         Just n'' | n'' /= n -> Just (redText "prop_adjust_inverse 2")
         _ -> case n *> invert p of
              Nothing -> Just (redText "prop_adjust_inverse 3")
              Just n'' | n'' /= n' -> Just (redText "prop_adjust_inverse 4")
              _ -> case invert p >* n' of
                   Nothing -> Just (redText "prop_adjust_inverse 5")
                   Just n'' | n'' /= n -> Just (redText "prop_adjust_inverse 6")
                   _ -> Nothing

instance (Show2 p, Show2 (PrimOf p)) => Show (Non p C(x)) where
    showsPrec d (Non cs p) = showParen (d > appPrec) $ showString "Non " .
                             showsPrec2 (appPrec + 1) cs . showString " " .
                             showsPrec2 (appPrec + 1) p

instance (Show2 p, Show2 (PrimOf p)) => Show1 (Non p) where
    showDict1 = ShowDictClass
