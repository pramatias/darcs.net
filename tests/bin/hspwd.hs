module Main where

import Directory ( getCurrentDirectory )

main = getCurrentDirectory >>= putStr
