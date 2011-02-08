{-# LANGUAGE ForeignFunctionInterface #-}
module System.Posix.IO where

#if mingw32_HOST_OS && __GLASGOW_HASKELL__ >= 612
import Foreign.C.String( withCWString )
#else
import Foreign.C.String ( withCString )
#endif

import Foreign.C.Error ( throwErrnoIfMinus1, throwErrnoIfMinus1_ )

import GHC.Handle ( fdToHandle )

import System.Posix.Internals ( c_open, c_close, c_dup2 )
import System.Posix.Types ( Fd(..), FileMode )
import System.IO ( Handle )

import Data.Bits ( (.|.) )


stdOutput :: Fd
stdOutput = Fd 1

stdError :: Fd
stdError = Fd 2

data OpenFileFlags = 
 OpenFileFlags {
  append :: Bool,
  exclusive :: Bool,
  noctty :: Bool,
  nonBlock :: Bool,
  trunc :: Bool
 }


-- Adapted from System.Posix.IO in ghc
#include <fcntl.h>

openFd :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
openFd name how maybe_mode off = do
#if mingw32_HOST_OS && __GLASGOW_HASKELL__ >= 612
  withCWString name $ \s -> do
#else
  withCString name $ \s -> do
#endif
   fd <- throwErrnoIfMinus1 "openFd" (c_open s all_flags mode_w)
   return (Fd fd)
 where
   all_flags = binary .|. creat .|. flags .|. open_mode
   flags =
    (if append off    then (#const O_APPEND)   else 0) .|.
    (if exclusive off then (#const O_EXCL)     else 0) .|.
    (if trunc off     then (#const O_TRUNC)    else 0)
   binary = (#const O_BINARY)
   (creat, mode_w) = maybe (0,0) (\x->((#const O_CREAT),x)) maybe_mode
   open_mode = case how of
                ReadOnly  -> (#const O_RDONLY)
                WriteOnly -> (#const O_WRONLY)
                ReadWrite -> (#const O_RDWR)

closeFd :: Fd -> IO ()
closeFd (Fd fd) = throwErrnoIfMinus1_ "closeFd" (c_close fd)


fdToHandle :: Fd -> IO Handle
fdToHandle fd = GHC.Handle.fdToHandle (fromIntegral fd)

dupTo :: Fd -> Fd -> IO Fd
dupTo (Fd fd1) (Fd fd2) = do
  r <- throwErrnoIfMinus1 "dupTo" (c_dup2 fd1 fd2)
  return (Fd r)

data OpenMode = ReadOnly | WriteOnly | ReadWrite

defaultFileFlags :: OpenFileFlags
defaultFileFlags = OpenFileFlags False False False False False


