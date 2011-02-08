{-# LANGUAGE ForeignFunctionInterface #-}

module System.Posix where

import Foreign.Ptr ( Ptr )
import Foreign.Storable ( peek )
import Foreign.C.Types ( CInt, CUInt, CULong, CTime )
import Foreign.C.String ( CString, withCString )
import Foreign.Marshal.Array ( withArray )
import Foreign.Marshal.Alloc ( alloca )

import System.Posix.Types ( EpochTime )
import System.IO ( Handle )

foreign import ccall "sys/utime.h _utime" c_utime :: CString -> Ptr CTime -> IO CInt

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()
setFileTimes path atime mtime = path `withCString` \s -> do
  withArray [atime,mtime] $ \p -> do c_utime s p
                                     return ()


foreign import ccall "time" c_ctime :: Ptr CTime -> IO CInt

epochTime :: IO EpochTime
epochTime = do
  alloca $ \p -> do c_ctime p
                    t <- peek p :: IO CTime
                    return t

foreign import stdcall "winbase.h SleepEx" c_SleepEx :: CULong -> CUInt -> IO CInt

sleep :: Integer -> IO CInt
sleep n = c_SleepEx (1000 * fromIntegral n) 1

handleToFd :: Handle -> IO Int
handleToFd _ = fail "handleToFd not supported!"

