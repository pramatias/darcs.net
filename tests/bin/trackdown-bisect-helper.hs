{-

Tool for construction of testing repository for trackdown testing
with bisect option. Written by Matthias Fischmann.

Usage:

./trackdown-bisect-helper '[0,1,1,1,0,0,0]'

This will generate a repository in which `grep -q 1 j` will first fail
three times, then succeed three times, then fail once if you unapply
patches with the linear implementation.

-}


import Control.Monad
import System.IO
import System
import System.Random
import Data.List
import Control.Exception


stamp i j = system ("echo " ++ show i ++ " > ./i") >>
            system ("echo " ++ show j ++ " > ./j") >>
            -- system ("sleep 1") >>
            hFlush stdout >>
            system ("darcs record --ignore-times -am '" ++ show i ++ "'")

generate :: [Int] -> IO ()
generate = mapM_ (uncurry stamp) . zip [1..]

main :: IO ()
main = do
  args <- getArgs
  let js = (read (head args)) :: [Int]
  generate js
