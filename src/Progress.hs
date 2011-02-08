-- Utility functions for tracking progress of long-running actions.

{-# LANGUAGE CPP #-}

#include "gadts.h"

module Progress ( beginTedious, endTedious, tediousSize,
                  debugMessage, debugFail, withoutProgress,
                  progress, progressKeepLatest, finishedOne,
                  finishedOneIO, progressList, minlist,
                  setProgressMode) where

import Prelude hiding (lookup)

import Control.Exception.Extensible ( onException )
import Control.Monad ( when )
import System.IO ( stdout, stderr, hFlush, hPutStr, hPutStrLn,
                   hSetBuffering, hIsTerminalDevice,
                   Handle, BufferMode(LineBuffering) )
import System.IO.Unsafe ( unsafePerformIO )
import Data.Char ( toLower )
import Data.Map ( Map, empty, adjust, insert, delete, lookup )
import Data.Maybe ( isJust )
import Control.Concurrent ( forkIO, threadDelay )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef, modifyIORef )

import Darcs.Global ( withDebugMode, debugMessage, putTiming, debugFail )

handleProgress :: IO ()
handleProgress = do threadDelay 1000000
                    handleMoreProgress "" 0

handleMoreProgress :: String -> Int -> IO ()
handleMoreProgress k n = withProgressMode $ \m ->
    if m then do s <- getProgressLast
                 mp <- getProgressData s
                 case mp of
                   Nothing -> do threadDelay 1000000
                                 handleMoreProgress k n
                   Just p -> do when (k /= s || n < sofar p) $ whenProgressMode $ printProgress s p
                                threadDelay 1000000
                                handleMoreProgress s (sofar p)
         else do threadDelay 1000000
                 handleMoreProgress k n

printProgress :: String -> ProgressData -> IO ()
printProgress k (ProgressData {sofar=s, total=Just t, latest=Just l}) =
    myput output output
        where output = (k++" "++show s++" done, "++show (t - s)++" queued. "++l)
printProgress k (ProgressData {latest=Just l}) =
    myput (k++" "++l) k
printProgress k (ProgressData {sofar=s, total=Just t}) | t >= s =
    myput (k++" "++show s++" done, "++show (t - s)++" queued") (k++" "++show s)
printProgress k (ProgressData {sofar=s}) =
    myput (k++" "++show s) k

myput :: String -> String -> IO ()
myput l s = withDebugMode $ \debugMode ->
            if debugMode
            then putTiming >> hPutStrLn stderr l
            else if '\n' `elem` l
                 then myput (takeWhile (/= '\n') l) s
                 else putTiming >> if length l < 80 then simpleput l
                                                    else simpleput (take 80 s)

{-# NOINLINE simpleput #-}
simpleput :: String -> IO ()
simpleput = unsafePerformIO $ mkhPutCr stderr

-- | @beginTedious k@ starts a tedious process and registers it in
-- '_progressData' with the key @k@. A tedious process is one for which we
-- want a progress indicator.
--
--  Wouldn't it be safer if it had type String -> IO
-- ProgressDataKey, so that we can ensure there is no collision?
-- What happens if you call beginTedious twice with the same string, without
-- calling endTedious in the meantime?
beginTedious :: String -> IO ()
beginTedious k = do debugMessage $ "Beginning " ++ (map toLower k)
                    setProgressData k $ ProgressData { sofar = 0,
                                                       latest = Nothing,
                                                       total = Nothing }

-- | @endTedious k@ unregisters the tedious process with key @k@, printing "Done" if such
-- a tedious process exists.
endTedious :: String -> IO ()
endTedious k = whenProgressMode $ do p <- getProgressData k
                                     modifyIORef _progressData (\(a,m) -> (a,delete k m))
                                     when (isJust p) $ debugMessage $ "Done "++
                                          (map toLower k)

tediousSize :: String -> Int -> IO ()
tediousSize k s = updateProgressData k uptot
    where uptot p = case total p of Just t -> seq ts $ p { total = Just ts }
                                        where ts = t + s
                                    Nothing -> p { total = Just s }

minlist :: Int
minlist = 4

progressList :: String -> [a] -> [a]
progressList _ [] = []
progressList k (x:xs) = if l < minlist then x:xs
                                       else startit x : pl xs
    where l = length (x:xs)
          startit y = unsafePerformIO $ do beginTedious k
                                           tediousSize k l
                                           return y
          pl [] = []
          pl [y] = unsafePerformIO $ do endTedious k
                                        return [y]
          pl (y:ys) = progress k y : pl ys


progress :: String -> a -> a
progress k a = unsafePerformIO $ progressIO k >> return a

progressIO :: String -> IO ()
progressIO "" = return ()
progressIO k = do updateProgressData k (\p -> p { sofar = sofar p + 1,
                                                  latest = Nothing })
                  putDebug k ""

progressKeepLatest :: String -> a -> a
progressKeepLatest k a = unsafePerformIO $ progressKeepLatestIO k >> return a

progressKeepLatestIO :: String -> IO ()
progressKeepLatestIO "" = return ()
progressKeepLatestIO k = do updateProgressData k (\p -> p {sofar = sofar p + 1})
                            putDebug k ""

finishedOne :: String -> String -> a -> a
finishedOne k l a = unsafePerformIO $ finishedOneIO k l >> return a

finishedOneIO :: String -> String -> IO ()
finishedOneIO "" _ = return ()
finishedOneIO k l = do updateProgressData k (\p -> p { sofar = sofar p + 1,
                                                       latest = Just l })
                       putDebug k l

putDebug :: String -> String -> IO ()
putDebug _ _ = return ()
--putDebug k "" = when (False && debugMode) $ hPutStrLn stderr $ "P: "++k
--putDebug k l = when (False && debugMode) $ hPutStrLn stderr $ "P: "++k++" : "++l

{-# NOINLINE _progressMode #-}
_progressMode :: IORef Bool
_progressMode = unsafePerformIO $ do hSetBuffering stderr LineBuffering
                                     newIORef True

{-# NOINLINE _progressData #-}
_progressData :: IORef (String, Map String ProgressData)
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
_progressData = unsafePerformIO $ do _ <- forkIO handleProgress
                                     newIORef ("", empty)

mkhPutCr :: Handle -> IO (String -> IO ())
mkhPutCr fe = do
  isTerm <- hIsTerminalDevice fe
  stdoutIsTerm <- hIsTerminalDevice stdout
  return $ if isTerm then \s -> do hPutStr fe $ '\r':s++"\r"
                                   hFlush fe
                                   let spaces = '\r':replicate (length s) ' '++"\r"
                                   hPutStr fe spaces
                                   when stdoutIsTerm $ hPutStr stdout spaces
                     else \s -> when (not $ null s) $ do hPutStrLn fe s
                                                         hFlush fe

setProgressMode :: Bool -> IO ()
setProgressMode m = writeIORef _progressMode m

withoutProgress :: IO a -> IO a
withoutProgress j = withProgressMode $ \m -> do debugMessage "Disabling progress reports..."
                                                setProgressMode False
                                                a <- j `onException` setProgressMode m
                                                if m then debugMessage "Reenabling progress reports."
                                                     else debugMessage "Leaving progress reports off."
                                                setProgressMode m
                                                return a

updateProgressData :: String -> (ProgressData -> ProgressData) -> IO ()
updateProgressData k f = whenProgressMode $ modifyIORef _progressData (\(_,m) -> (k,adjust f k m))

setProgressData :: String -> ProgressData -> IO ()
setProgressData k p = whenProgressMode $ modifyIORef _progressData (\(a,m) -> (a,insert k p m))

getProgressData :: String -> IO (Maybe ProgressData)
getProgressData k = withProgressMode $ \p -> if p then (lookup k . snd) `fmap` readIORef _progressData
                                                  else return Nothing

getProgressLast :: IO String
getProgressLast = withProgressMode $ \p -> if p then fst `fmap` readIORef _progressData
                                                else return ""

whenProgressMode :: IO a -> IO ()
whenProgressMode j = withProgressMode $ const $ j >> return ()

withProgressMode :: (Bool -> IO a) -> IO a
withProgressMode j = readIORef _progressMode >>= j

data ProgressData = ProgressData { sofar :: !Int,
                                   latest :: !(Maybe String),
                                   total :: !(Maybe Int)}
