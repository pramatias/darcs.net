{-# LANGUAGE CPP #-}

module Darcs.Witnesses.Show(ShowDict(..), showD, showListD, showsPrecD,
                            Show1(..), Show2(..), show1, showsPrec1, show2, showsPrec2, showOp2, appPrec) where

#include "gadts.h"

data ShowDict a where
    ShowDictClass :: Show a => ShowDict a
    ShowDictRecord :: (Int -> a -> ShowS) -> (a -> String) -> ([a] -> ShowS) -> ShowDict a

showsPrecD :: ShowDict a -> Int -> a -> ShowS
showsPrecD ShowDictClass       = showsPrec
showsPrecD (ShowDictRecord showsPrecR _ _) = showsPrecR

showD :: ShowDict a -> a -> String
showD ShowDictClass       = show
showD (ShowDictRecord _ showR _) = showR

showListD :: ShowDict a -> [a] -> ShowS
showListD ShowDictClass       = showList
showListD (ShowDictRecord _ _ showListR) = showListR

class Show1 a where
    showDict1 :: ShowDict (a C(x))

showsPrec1 :: Show1 a => Int -> a C(x) -> ShowS
showsPrec1 = showsPrecD showDict1

show1 :: Show1 a => a C(x) -> String
show1 = showD showDict1

class Show2 a where
    showDict2 :: ShowDict (a C(x y))

showsPrec2 :: Show2 a => Int -> a C(x y) -> ShowS
showsPrec2 = showsPrecD showDict2

show2 :: Show2 a => a C(x y) -> String
show2 = showD showDict2

showOp2 :: (Show2 a, Show2 b) => Int -> String -> Int -> a C(w x) -> b C(y z) -> String -> String
showOp2 prec opstr d x y = showParen (d > prec) $ showsPrec2 (prec + 1) x .
                          showString opstr . showsPrec2 (prec + 1) y

appPrec :: Int
appPrec = 10
