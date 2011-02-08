-- Copyright (C) 2002-2003 David Roundy
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

module Main (main) where

import Prelude

import System.IO ( stdin, stdout, stderr, hSetBinaryMode )
import Control.Monad ( forM_ )
import System.Exit ( exitWith, ExitCode(..) )
import System.Environment ( getArgs )
import Control.Exception.Extensible ( AssertionFailed(..), handle )

import Darcs.RunCommand ( runTheCommand )
import Darcs.Flags ( DarcsFlag(Verbose) )
import Darcs.Commands.Help ( helpCmd, listAvailableCommands, printVersion, commandControlList )
import Darcs.SignalHandler ( withSignalsHandled )
import Version ( version, context, builddeps )
import Darcs.Global ( withAtexit, atexit )
import Darcs.Repository( reportBadSources )
import Preproc( preprocMain )
import Exec ( ExecException(..) )
#include "impossible.h"

execExceptionHandler :: ExecException -> IO a
execExceptionHandler (ExecException cmd args redirects reason) =
    do putStrLn $ "Failed to execute external command: " ++ unwords (cmd:args) ++ "\n"
                    ++ "Lowlevel error: " ++ reason ++ "\n"
                    ++ "Redirects: " ++ show redirects ++"\n"
       exitWith $ ExitFailure 3

main :: IO ()
main = withAtexit $ withSignalsHandled $
  handle execExceptionHandler $
  handle (\(AssertionFailed e) -> bug e) $ do
  atexit reportBadSources
  argv <- getArgs
  case argv of
    -- User called "darcs" without arguments.
    []                  -> printVersion >> helpCmd [] []
    -- User called "darcs --foo" for some special foo.
    ["-h"]              -> helpCmd [] []
    ["--help"]          -> helpCmd [] []
    ["--overview"]      -> helpCmd [Verbose] []
    ["--commands"]      -> listAvailableCommands
    ["-v"]              -> putStrLn version
    ["--version"]       -> putStrLn version
    ["--exact-version"] -> do
              putStrLn $ "darcs compiled on "++__DATE__++", at "++__TIME__
              putStrLn context
              putStrLn $ "Compiled with:\n"
              putStr builddeps
    ("--preprocess-manual":rest) -> preprocMain rest
    -- User called a normal darcs command, "darcs foo [args]".
    _ -> do
      forM_ [stdout, stdin, stderr] $ \h -> hSetBinaryMode h True
      runTheCommand commandControlList (head argv) (tail argv)
