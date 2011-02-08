#include "gadts.h"
module Darcs.Witnesses.Eq (
  EqCheck(..), MyEq(..), isIsEq
) where

import Darcs.Witnesses.Unsafe ( unsafeCoerceP )

-- |'EqCheck' is used to pass around evidence (or lack thereof) of
-- two witness types being equal.
data EqCheck C(a b) where
    IsEq :: EqCheck C(a a)
    NotEq :: EqCheck C(a b)

instance Eq (EqCheck C(a b)) where
    IsEq == IsEq = True
    NotEq == NotEq = True
    _ == _ = False

instance Show (EqCheck C(a b)) where
    show IsEq = "IsEq"
    show NotEq = "NotEq"

-- |An witness aware equality class.
-- A minimal definition defines any one of 'unsafeCompare', '=\/=' and '=/\='.
class MyEq p where
    -- |It is unsafe to define a class instance via this method, because
    -- if it returns True then the default implementations of '=\/=' and '=/\='
    -- will coerce the equality of two witnesses.
    --
    -- Calling this method is safe, although '=\/=' or '=/\=' would be better
    -- choices as it is not usually meaningul to compare two patches that
    -- don't share either a starting or an ending context
    unsafeCompare :: p C(a b) -> p C(c d) -> Bool
    unsafeCompare a b = IsEq == (a =/\= unsafeCoerceP b)

    -- |Compare two things with the same starting witness. If the things
    -- compare equal, evidence of the ending witnesses being equal will
    -- be returned.
    (=\/=) :: p C(a b) -> p C(a c) -> EqCheck C(b c)
    a =\/= b | unsafeCompare a b = unsafeCoerceP IsEq
             | otherwise = NotEq

    -- |Compare two things with the same ending witness. If the things
    -- compare equal, evidence of the starting witnesses being equal will
    -- be returned.
    (=/\=) :: p C(a c) -> p C(b c) -> EqCheck C(a b)
    a =/\= b | IsEq == (a =\/= unsafeCoerceP b) = unsafeCoerceP IsEq
             | otherwise = NotEq

infix 4 =\/=, =/\=

isIsEq :: EqCheck C(a b) -> Bool
isIsEq IsEq = True
isIsEq NotEq = False