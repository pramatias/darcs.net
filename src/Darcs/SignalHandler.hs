-- Copyright (C) 2003 David Roundy
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

{-# LANGUAGE CPP, DeriveDataTypeable #-}

module Darcs.SignalHandler ( withSignalsHandled, withSignalsBlocked,
                             catchInterrupt, catchNonSignal,
                             tryNonSignal, stdoutIsAPipe ) where

import Prelude hiding ( catch )

import System.IO.Error ( isUserError, ioeGetErrorString, ioeGetFileName )
import System.Exit ( exitWith, ExitCode ( ExitFailure ) )
import Control.Concurrent ( ThreadId, myThreadId )
import Control.Exception.Extensible
            ( catch, throw, throwTo, block,
              Exception(..), SomeException(..), IOException )
import System.Posix.Files ( getFdStatus, isNamedPipe )
import System.Posix.IO ( stdOutput )
import Data.Typeable ( Typeable, cast )
import Data.List ( isPrefixOf )
import System.IO ( hPutStrLn, stderr )
import Control.Monad ( when )

import Workaround ( installHandler, raiseSignal, Handler(..), Signal,
                    sigINT, sigHUP, sigABRT, sigALRM, sigTERM, sigPIPE )
#ifdef WIN32
import CtrlC ( withCtrlCHandler )
#endif

stdoutIsAPipe :: IO Bool
stdoutIsAPipe
 = catch
        (do stat <- getFdStatus stdOutput
            return (isNamedPipe stat))
        (\(_ :: IOException) -> return False)

withSignalsHandled :: IO a -> IO a
newtype SignalException = SignalException Signal deriving (Show, Typeable)

instance Exception SignalException where
   toException e = SomeException e
   fromException (SomeException e) = cast e

withSignalsHandled job = do
    thid <- myThreadId
    mapM_ (ih thid) [sigINT, sigHUP, sigABRT, sigTERM, sigPIPE]
    catchUserErrors (job' thid `catchSignal` defaults)
                    die_with_string
    where defaults s | s == sigINT = ew s "Interrupted!"
                     | s == sigHUP = ew s "HUP"
                     | s == sigABRT = ew s "ABRT"
                     | s == sigTERM = ew s "TERM"
                     | s == sigPIPE = exitWith $ ExitFailure $ 1
                     | otherwise = ew s "Unhandled signal!"
          ew sig s = do hPutStrLn stderr $ ("withSignalsHandled: " ++ s)
                        resethandler sig
                        raiseSignal sig -- ensure that our caller knows how we died
                        exitWith $ ExitFailure $ 1
          die_with_string e | "STDOUT" `isPrefixOf` e =
                do is_pipe <- stdoutIsAPipe
                   when (not is_pipe) $
                        hPutStrLn stderr $ "\ndarcs failed:  "++drop 6 e
                   exitWith $ ExitFailure $ 2
          die_with_string e = do hPutStrLn stderr $ "\ndarcs failed:  "++e
                                 exitWith $ ExitFailure $ 2
#ifdef WIN32
          job' thid =
             withCtrlCHandler (throwTo thid $ SignalException sigINT) job
#else
          job' _ = job
#endif

resethandler :: Signal -> IO ()
-- Warning:  A do-notation statement discarded a result of type Handler.
resethandler s = do _ <- installHandler s Default Nothing
                    return ()

ih :: ThreadId -> Signal -> IO ()
ih thid s =
-- Warning:  A do-notation statement discarded a result of type Handler.
  do _ <- installHandler s (Catch $ throwTo thid $ SignalException s) Nothing
     return ()

catchSignal :: IO a -> (Signal -> IO a) -> IO a
catchSignal job handler =
    job `catch` (\(SignalException sig) -> handler sig)

-- catchNonSignal is a drop-in replacement for Control.Exception.catch, which allows
-- us to catch anything but a signal.  Useful for situations where we want
-- don't want to inhibit ctrl-C.

catchNonSignal :: IO a -> (SomeException -> IO a) -> IO a
catchNonSignal comp handler = catch comp handler'
    where handler' se =
           case fromException se :: Maybe SignalException of
             Nothing -> handler se
             Just _ -> throw se

catchInterrupt :: IO a -> IO a -> IO a
catchInterrupt job handler =
    job `catchSignal` h
        where h s | s == sigINT = handler
                  | otherwise   = throw (SignalException s)

tryNonSignal :: IO a -> IO (Either SomeException a)
tryNonSignal j = (Right `fmap` j) `catchNonSignal` \e -> return (Left e)

catchUserErrors :: IO a -> (String -> IO a) -> IO a
catchUserErrors comp handler = catch comp handler'
  where handler' ioe
         | isUserError ioe                       = handler (ioeGetErrorString ioe)
         | ioeGetFileName ioe == Just "<stdout>" = handler ("STDOUT" ++ ioeGetErrorString ioe)
         | otherwise                             = throw ioe

withSignalsBlocked :: IO () -> IO ()
withSignalsBlocked job = (block job) `catchSignal` couldnt_do
    where couldnt_do s | s == sigINT = oops "interrupt"
                       | s ==  sigHUP = oops "HUP"
                       | s ==  sigABRT = oops "ABRT"
                       | s ==  sigALRM = oops "ALRM"
                       | s ==  sigTERM = oops "TERM"
                       | s ==  sigPIPE = return ()
                       | otherwise = oops "unknown signal"
          oops s = hPutStrLn stderr $ "Couldn't handle " ++ s ++
                   " since darcs was in a sensitive job."
