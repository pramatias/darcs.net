{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- |
-- Module:    Data.Digest.SHA256
-- Copyright: Zooko O'Whielacronx
-- License:   GPL
--
-- Stability: experimental

-- ByteString-based, zero-copying binding to Crypto++'s sha interface

-- thanks to Don Stewart <dons@galois.com>, Matthew Sackman
-- <matthew@wellquite.org>, Brian O'Sullivan, lispy, Adam Langley

module Crypt.SHA256 ( sha256sum ) where

import Foreign
import Foreign.C.Types
import Numeric (showHex)
import Foreign.C.String ( withCString )
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import qualified Data.ByteString as B

sha256sum :: B.ByteString -> String
sha256sum p = unsafePerformIO $
              withCString (take 64 $ repeat 'x') $ \digestCString ->
              unsafeUseAsCStringLen p $ \(ptr,n) ->
              do let digest = castPtr digestCString :: Ptr Word8
                 c_sha256 ptr (fromIntegral n) digest
                 go digest 0 []
  where -- print it in 0-padded hex format
        go :: Ptr Word8 -> Int -> [String] -> IO String
        go q n acc | seq q n >= 32 = return $ concat (reverse acc)
                   | otherwise = do w <- peekElemOff q n
                                    go q (n+1) (draw w : acc)
        draw w = case showHex w [] of
                 [x] -> ['0', x]
                 x   -> x

-- void sha256sum(const unsigned char *d, size_t n, unsigned char *md);
--
foreign import ccall unsafe "sha2.h sha256" c_sha256
    :: Ptr CChar -> CSize -> Ptr Word8 -> IO ()

