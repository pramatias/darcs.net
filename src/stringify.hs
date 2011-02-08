module Main where

import System.Environment (getArgs)

main :: IO ()
main = do -- Module header
  [modulename, varname] <- getArgs
  putStrLn $ "module " ++ modulename ++ " (" ++ varname ++ ") where"
  -- Type sig so we can compile -Wall -Werror
  putStrLn $ varname ++ " :: String"
  -- And now quote the stdin so it's a String
  ls <- getContents
  putStrLn $ varname ++ " =\"" ++ concatMap escape ls ++ "\""
    where
      escape '\n' = "\\n\\\n\\"
      escape '"'  = "\\\""
      escape '\\' = "\\\\"
      escape c = [c]