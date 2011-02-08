% Copyright (C) 2009 Petr Rockai
%
% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation
% files (the "Software"), to deal in the Software without
% restriction, including without limitation the rights to use, copy,
% modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

\darcsCommand{show index}
\begin{code}
{-# LANGUAGE CPP #-}
#include "gadts.h"
module Darcs.Commands.ShowIndex ( showIndex
                                , showPristineCmd -- for alias
                                ) where
import Darcs.Arguments ( DarcsFlag(..), workingRepoDir,
                        files, directories, nullFlag )
import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Repository ( amInRepository, withRepository, RepoJob(..), readIndex )
import Darcs.Repository.State ( readRecorded )

import Storage.Hashed( floatPath )
import Storage.Hashed.Hash( encodeBase16, Hash( NoHash ) )
import Storage.Hashed.Tree( list, expand, itemHash, Tree, TreeItem( SubTree ) )
import Storage.Hashed.Index( updateIndex )
import Storage.Hashed.AnchoredPath( anchorPath )

import qualified Data.ByteString.Char8 as BS

showIndex :: DarcsCommand
showIndex = DarcsCommand {
  commandProgramName = "darcs",
  commandName = "index",
  commandDescription = "Dump contents of working tree index.",
  commandHelp =
      "The `darcs show index' command lists all version-controlled files and " ++
      "directories along with their hashes as stored in _darcs/index. " ++
      "For files, the fields correspond to file size, sha256 of the current " ++
      "file content and the filename.",
  commandExtraArgs = 0,
  commandExtraArgHelp = [],
  commandCommand = showIndexCmd,
  commandPrereq = amInRepository,
  commandGetArgPossibilities = return [],
  commandArgdefaults = nodefaults,
  commandAdvancedOptions = [],
  commandBasicOptions = [files, directories, nullFlag, workingRepoDir] }

dump :: [DarcsFlag] -> Tree IO -> IO ()
dump opts tree = do
  let line | NullFlag `elem` opts = \t -> putStr t >> putChar '\0'
           | otherwise = putStrLn
      output (p, i) = do
        let hash = case itemHash i of
                     NoHash -> "(no hash available)"
                     h -> BS.unpack $ encodeBase16 h
            path = anchorPath "" p
            isdir = case i of
                      SubTree _ -> "/"
                      _ -> ""
        line $ hash ++ " " ++ path ++ isdir
  x <- expand tree
  mapM_ output $ (floatPath ".", SubTree x) : list x

showIndexCmd :: [DarcsFlag] -> [String] -> IO ()
showIndexCmd opts _ = withRepository opts $ RepoJob $ \repo -> do
  readIndex repo >>= updateIndex >>= dump opts

showPristineCmd :: [DarcsFlag] -> [String] -> IO ()
showPristineCmd opts _ = withRepository opts $ RepoJob $ \repo -> do
  readRecorded repo >>= dump opts

\end{code}
