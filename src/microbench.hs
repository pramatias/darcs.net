-- Copyright (C) 2008 David Roundy
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
-- along with this program;  see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Main (main) where

import System ( getArgs )

import qualified Data.ByteString as B
import SHA1 ( sha1PS )
import Crypt.SHA256 ( sha256sum )

main :: IO ()
main = do args <- getArgs
          case args of
            ("sha1":fs) -> mapM_ sha1 fs
                where sha1 f = do x <- sloppyReadFilePS f
                                  putStrLn $ f ++ ':' : sha1PS x
            ("sha2":fs) -> mapM_ sha2 fs
                where sha2 f = do x <- sloppyReadFilePS f
                                  putStrLn $ f ++ ':' : sha256sum x
            _ -> fail $ unwords $ "Invalid arguments:  ":args

sloppyReadFilePS :: String -> IO B.ByteString
sloppyReadFilePS f = B.readFile f `catch` \e -> do putStrLn (show e); return B.empty
