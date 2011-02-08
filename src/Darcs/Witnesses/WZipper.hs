-- Copyright (C) 2009 Florent Becker
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

#include "gadts.h"
module Darcs.Witnesses.WZipper ( FZipper(..), focus, leftmost, left
                               , rightmost, right, jokers, clowns
                               , flToZipper, lengthFZ, nullFZ
                               , toEnd
                               )
where
import Darcs.Witnesses.Ordered ( FL(..), RL(..), nullFL, nullRL
                               , lengthFL, lengthRL, (+<+)
                               , reverseFL
                               )
import Darcs.Witnesses.Sealed(Sealed2(..), Sealed(..), FlippedSeal(..))

-- forward zipper
data FZipper a C(x z) where
    FZipper :: RL a C(x y) -> FL a C(y z) -> FZipper a C(x z)

-- Constructors
flToZipper :: FL a C(x y) -> FZipper a C(x y)
flToZipper l = FZipper NilRL l

--destructors
nullFZ :: FZipper a C(x y) -> Bool
nullFZ (FZipper l r) = nullRL l && nullFL r

lengthFZ :: FZipper a C(x y) -> Int
lengthFZ (FZipper l r) = lengthRL l + lengthFL r

focus :: FZipper a C(x y) -> Maybe (Sealed2 a)
focus (FZipper _ (x :>: _)) = Just $ Sealed2 x
focus _ = Nothing

-- | \"Clowns to the left of me, jokers to the right.  Here I am, stuck
--   in the middle of you\"
--   <http://en.wikipedia.org/wiki/Stuck_in_the_Middle>
clowns :: FZipper a C(x y) -> Sealed ((RL a) C(x))
clowns (FZipper l _) = Sealed l

-- | See 'clowns'
jokers :: FZipper a C(x y) -> FlippedSeal (FL a) C(y)
jokers (FZipper _ r) = FlippedSeal r

rightmost :: FZipper p C(x y) -> Bool
rightmost (FZipper _ NilFL) = True
rightmost _ = False

right :: FZipper p C(x y) -> FZipper p C(x y)
right (FZipper l (b:>:r)) = FZipper (b :<: l) r
right x@(FZipper _ NilFL) = x

leftmost :: FZipper p C(x y) -> Bool
leftmost (FZipper NilRL _) = True
leftmost _ = False

left :: FZipper p C(x y) -> FZipper p C(x y)
left (FZipper (b :<: l) r) = FZipper l (b :>: r)
left x@(FZipper NilRL _) = x

toEnd :: FZipper p C(x y) -> FZipper p C(x y)
toEnd (FZipper l r) = FZipper (reverseFL r +<+ l) NilFL

