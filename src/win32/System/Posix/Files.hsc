{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.Posix.Files( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
                               , getFdStatus, getFileStatus, getSymbolicLinkStatus
                               , modificationTime, setFileMode, fileSize, fileMode
                               , stdFileMode, linkCount, createLink ) where

import System.PosixCompat.Files( isNamedPipe, isDirectory, isRegularFile, isSymbolicLink
                               , getFdStatus, getFileStatus, getSymbolicLinkStatus
                               , modificationTime, setFileMode, fileSize, fileMode
                               , stdFileMode, FileStatus )

import Foreign.C.String( CWString, withCWString )
import Foreign.C.Error( throwErrnoPathIf_ )
import Foreign.Ptr( Ptr, nullPtr )
import Foreign.C( CInt )

linkCount :: FileStatus -> Int
linkCount _ = 1

#define _WIN32_WINNT 0x0500
foreign import stdcall "winbase.h CreateHardLinkW" c_CreateHardLink :: CWString -> CWString -> Ptr a -> IO CInt

createLink :: FilePath -> FilePath -> IO ()
createLink old new = withCWString old $ \c_old -> withCWString new $ \c_new ->
	throwErrnoPathIf_ (==0) "createLink" new $
		c_CreateHardLink c_new c_old nullPtr
