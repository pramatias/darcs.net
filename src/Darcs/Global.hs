-- Copyright (C) 2005 Tomasz Zielonka
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
-- | This was originally Tomasz Zielonka's AtExit module, slightly generalised
-- to include global variables.  Here, we attempt to cover broad, global
-- features, such as exit handlers.  These features slightly break the Haskellian
-- purity of darcs, in favour of programming convenience.
module Darcs.Global ( atexit, withAtexit,
                      sshControlMasterDisabled, setSshControlMasterDisabled,
                      timingsMode, setTimingsMode,
                      whenDebugMode, withDebugMode, setDebugMode,
                      debugMessage, debugFail, putTiming,
                      addCRCWarning, getCRCWarnings, resetCRCWarnings,
                      addBadSource, getBadSourcesList, isBadSource, darcsdir,
                      isReachableSource, addReachableSource
    ) where

import Control.Monad ( when )
import Control.Concurrent.MVar
import Control.Exception.Extensible (bracket_, catch, block, unblock, SomeException)
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Data.IORef ( modifyIORef )
import System.IO.Unsafe (unsafePerformIO)
import System.IO (hPutStrLn, hPutStr, stderr)
import System.Time ( calendarTimeToString, toCalendarTime, getClockTime )
import Prelude hiding (catch)

{-# NOINLINE atexitActions #-}
atexitActions :: MVar (Maybe [IO ()])
atexitActions = unsafePerformIO (newMVar (Just []))

-- | Registers an IO action to run just before darcs exits.  Useful
-- for removing temporary files and directories, for example.
atexit :: IO () -> IO ()
atexit action = do
    modifyMVar_ atexitActions $ \ml -> do
        case ml of
            Just l -> do
                return (Just (action : l))
            Nothing -> do
                hPutStrLn stderr "It's too late to use atexit"
                return Nothing

withAtexit :: IO a -> IO a
withAtexit prog = do
    bracket_
        (return ())
        exit
        prog
  where
    exit = block $ do
        Just actions <- swapMVar atexitActions Nothing
        -- from now on atexit will not register new actions
        mapM_ runAction actions
    runAction action = do
        catch (unblock action) $ \(exn :: SomeException) -> do
            hPutStrLn stderr $ "Exception thrown by an atexit registered action:"
            hPutStrLn stderr $ show exn


-- Write-once-read-many global variables make it easier to implement flags, such
-- as --no-ssh-cm.  Using global variables reduces the number of parameters
-- that we have to pass around, but it is rather unsafe and should be used sparingly.

{-# NOINLINE _debugMode #-}
_debugMode :: IORef Bool
_debugMode = unsafePerformIO $ newIORef False

setDebugMode :: IO ()
setDebugMode = writeIORef _debugMode True

whenDebugMode :: IO () -> IO ()
whenDebugMode j = do b <- readIORef _debugMode
                     when b j

withDebugMode :: (Bool -> IO a) -> IO a
withDebugMode j = readIORef _debugMode >>= j


debugMessage :: String -> IO ()
debugMessage m = whenDebugMode $ do putTiming; hPutStrLn stderr m

debugFail :: String -> IO a
debugFail m = debugMessage m >> fail m

putTiming :: IO ()
putTiming = when timingsMode $ do t <- getClockTime >>= toCalendarTime
                                  hPutStr stderr (calendarTimeToString t++": ")

{-# NOINLINE _timingsMode #-}
_timingsMode :: IORef Bool
_timingsMode = unsafePerformIO $ newIORef False

setTimingsMode :: IO ()
setTimingsMode = writeIORef _timingsMode True

{-# NOINLINE timingsMode #-}
timingsMode :: Bool
timingsMode = unsafePerformIO $ readIORef _timingsMode

{-# NOINLINE _sshControlMasterDisabled #-}
_sshControlMasterDisabled :: IORef Bool
_sshControlMasterDisabled = unsafePerformIO $ newIORef False

setSshControlMasterDisabled :: IO ()
setSshControlMasterDisabled = writeIORef _sshControlMasterDisabled True

{-# NOINLINE sshControlMasterDisabled #-}
sshControlMasterDisabled :: Bool
sshControlMasterDisabled = unsafePerformIO $ readIORef _sshControlMasterDisabled

type CRCWarningList = [FilePath]
{-# NOINLINE _crcWarningList #-}
_crcWarningList :: IORef CRCWarningList
_crcWarningList = unsafePerformIO $ newIORef []

addCRCWarning :: FilePath -> IO ()
addCRCWarning fp = modifyIORef _crcWarningList (fp:)

getCRCWarnings :: IO [FilePath]
getCRCWarnings = readIORef _crcWarningList

resetCRCWarnings :: IO ()
resetCRCWarnings = writeIORef _crcWarningList []

{- NOINLINE _badSourcesList -}
_badSourcesList :: IORef [String]
_badSourcesList = unsafePerformIO $ newIORef []

addBadSource :: String -> IO ()
addBadSource cache = modifyIORef _badSourcesList (cache:)

getBadSourcesList :: IO [String]
getBadSourcesList = readIORef _badSourcesList

isBadSource :: IO (String -> Bool)
isBadSource = do badSources <- getBadSourcesList
                 return (`elem` badSources)

{- NOINLINE _reachableSourcesList -}
_reachableSourcesList :: IORef [String]
_reachableSourcesList = unsafePerformIO $ newIORef []

addReachableSource :: String -> IO ()
addReachableSource src = modifyIORef _reachableSourcesList (src:)

getReachableSources :: IO [String]
getReachableSources = readIORef _reachableSourcesList

isReachableSource :: IO (String -> Bool)
isReachableSource =  do reachableSources <- getReachableSources
                        return (`elem` reachableSources)

darcsdir :: String
darcsdir = "_darcs"
