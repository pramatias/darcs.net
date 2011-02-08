{-# LANGUAGE BangPatterns, ForeignFunctionInterface, CPP, ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  ByteStringUtils
-- Copyright   :  (c) The University of Glasgow 2001,
--                    David Roundy 2003-2005
-- License : GPL (I'm happy to also license this file BSD style but don't
--           want to bother distributing two license files with darcs.
--
-- Maintainer  :  droundy@abridgegame.org
-- Stability   :  experimental
-- Portability :  portable
--
-- GZIp and MMap IO for ByteStrings, encoding utilities, and miscellaneous
-- functions for Data.ByteString
--

module ByteStringUtils (

        unsafeWithInternals,
        unpackPSFromUTF8,
        packStringToUTF8,

        -- IO with mmap or gzip
        gzReadFilePS,
        mmapFilePS,
        gzWriteFilePS,
        gzWriteFilePSs,
        gzReadStdin,

        -- gzip handling
        isGZFile,
        gzDecompress,

        -- list utilities
        dropSpace,
        breakSpace,
        linesPS,
        unlinesPS,
        hashPS,
        breakFirstPS,
        breakLastPS,
        substrPS,
        readIntPS,
        isFunky,
        fromHex2PS,
        fromPS2Hex,
        betweenLinesPS,
        breakAfterNthNewline,
        breakBeforeNthNewline,
        intercalate,

        -- encoding and unicode utilities
        decodeLocale,
        encodeLocale,
        decodeString
    ) where

import Prelude hiding ( catch )
import qualified Data.ByteString            as B
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Internal   as BI
import Data.ByteString (intercalate)
import Data.ByteString.Internal (fromForeignPtr)

#if defined (HAVE_MMAP)
import Control.Exception.Extensible ( catch, SomeException )
#endif
import System.IO
import System.IO.Unsafe         ( unsafePerformIO )
import System.Console.Haskeline ( runInputT, defaultSettings )
import System.Console.Haskeline.Encoding ( decode, encode )

import Foreign.Storable         ( peekElemOff, peek )
import Foreign.Marshal.Array    ( advancePtr )
import Foreign.C.Types          ( CInt )

import Data.Bits                ( rotateL )
import Data.Char                ( ord, isSpace )
import Data.Word                ( Word8 )
import Data.Int                 ( Int32 )
import qualified Data.Text as T ( pack, unpack )
import Data.Text.Encoding       ( encodeUtf8, decodeUtf8With )
import Data.Text.Encoding.Error ( lenientDecode )
import Control.Monad            ( when )

import Foreign.Ptr              ( plusPtr, Ptr )
import Foreign.ForeignPtr       ( withForeignPtr )

#ifdef DEBUG_PS
import Foreign.ForeignPtr       ( addForeignPtrFinalizer )
import Foreign.Ptr              ( FunPtr )
#endif

import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZ
import qualified Codec.Compression.Zlib.Internal as ZI
import Darcs.Global ( addCRCWarning )

#ifdef HAVE_MMAP
import System.IO.MMap( mmapFileByteString )
import System.Mem( performGC )
import System.Posix.Files( fileSize, getSymbolicLinkStatus )
#endif

-- -----------------------------------------------------------------------------
-- obsolete debugging code

-- -----------------------------------------------------------------------------
-- unsafeWithInternals

-- | Do something with the internals of a PackedString. Beware of
-- altering the contents!
unsafeWithInternals :: B.ByteString -> (Ptr Word8 -> Int -> IO a) -> IO a
unsafeWithInternals ps f
 = case BI.toForeignPtr ps of
   (fp,s,l) -> withForeignPtr fp $ \p -> f (p `plusPtr` s) l

-- | readIntPS skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the PackedString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise it
-- just returns the int read, along with a B.ByteString containing the
-- remainder of its input.

readIntPS :: B.ByteString -> Maybe (Int, B.ByteString)
readIntPS = BC.readInt . BC.dropWhile isSpace

-- -----------------------------------------------------------------------------
-- Destructor functions (taking PackedStrings apart)

-- | Decodes a 'ByteString' containing UTF-8 to a 'String'. Decoding errors are
--   flagged with the U+FFFD character.
unpackPSFromUTF8 :: B.ByteString -> String
unpackPSFromUTF8  = T.unpack . decodeUtf8With lenientDecode

packStringToUTF8 :: String -> B.ByteString
packStringToUTF8 = encodeUtf8 . T.pack

------------------------------------------------------------------------
-- A reimplementation of Data.ByteString.Char8.dropSpace, but
-- specialised to darcs' need for a 4 way isspace.
--
-- TODO: if it is safe to use the expanded definition of isSpaceWord8
-- provided by Data.ByteString.Char8, then all this can go.

-- A locale-independent isspace(3) so patches are interpreted the same everywhere.
-- ((c) == ' ' || (c) == '\t' || (c) == '\n' || (c) == '\r')
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w =
    w == 0x20 ||    -- ' '
    w == 0x09 ||    -- '\t'
    w == 0x0A ||    -- '\n'
    w == 0x0D       -- '\r'
{-# INLINE isSpaceWord8 #-}

firstnonspace :: Ptr Word8 -> Int -> Int -> IO Int
firstnonspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if isSpaceWord8 w then firstnonspace ptr (n+1) m else return n

firstspace :: Ptr Word8 -> Int -> Int -> IO Int
firstspace !ptr !n !m
    | n >= m    = return n
    | otherwise = do w <- peekElemOff ptr n
                     if (not . isSpaceWord8) w then firstspace ptr (n+1) m else return n

-- | 'dropSpace' efficiently returns the 'ByteString' argument with
-- white space Chars removed from the front. It is more efficient than
-- calling dropWhile for removing whitespace. I.e.
--
-- > dropWhile isSpace == dropSpace
--
dropSpace :: B.ByteString -> B.ByteString
dropSpace (BI.PS x s l) = BI.inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstnonspace (p `plusPtr` s) 0 l
    return $! if i == l then B.empty else BI.PS x (s+i) (l-i)
{-# INLINE dropSpace #-}

-- | 'breakSpace' returns the pair of ByteStrings when the argument is
-- broken at the first whitespace byte. I.e.
--
-- > break isSpace == breakSpace
--
breakSpace :: B.ByteString -> (B.ByteString,B.ByteString)
breakSpace (BI.PS x s l) = BI.inlinePerformIO $ withForeignPtr x $ \p -> do
    i <- firstspace (p `plusPtr` s) 0 l
    return $! case () of {_
        | i == 0    -> (B.empty, BI.PS x s l)
        | i == l    -> (BI.PS x s l, B.empty)
        | otherwise -> (BI.PS x s i, BI.PS x (s+i) (l-i))
    }
{-# INLINE breakSpace #-}

------------------------------------------------------------------------

{-# INLINE isFunky #-}
isFunky :: B.ByteString -> Bool
isFunky ps = case BI.toForeignPtr ps of
   (x,s,l) ->
    unsafePerformIO $ withForeignPtr x $ \p->
    (/=0) `fmap` has_funky_char (p `plusPtr` s) (fromIntegral l)

foreign import ccall unsafe "fpstring.h has_funky_char" has_funky_char
    :: Ptr Word8 -> CInt -> IO CInt

------------------------------------------------------------------------

-- ByteString rewrites break (=='x') to breakByte 'x'
--  break ((==) x) = breakChar x
--  break (==x) = breakChar x
--

{-
{-# INLINE breakOnPS #-}
breakOnPS :: Char -> B.ByteString -> (B.ByteString, B.ByteString)
breakOnPS c p = case BC.elemIndex c p of
                Nothing -> (p, BC.empty)
                Just n  -> (B.take n p, B.drop n p)
-}

{-# INLINE hashPS #-}
hashPS :: B.ByteString -> Int32
hashPS ps =
   case BI.toForeignPtr ps of
   (x,s,l) ->
    unsafePerformIO $ withForeignPtr x $ \p->
    do hash (p `plusPtr` s) l

hash :: Ptr Word8 -> Int -> IO Int32
hash ptr len = f (0 :: Int32) ptr len
 where f h _ 0 = return h
       f h p n = do x <- peek p
                    let !h' =  (fromIntegral x) + (rotateL h 8)
                    f h' (p `advancePtr` 1) (n-1)

{-# INLINE substrPS #-}
substrPS :: B.ByteString -> B.ByteString -> Maybe Int
substrPS tok str
    | B.null tok = Just 0
    | B.length tok > B.length str = Nothing
    | otherwise = do n <- BC.elemIndex (BC.head tok) str
                     let ttok = B.tail tok
                         reststr = B.drop (n+1) str
                     if ttok == B.take (B.length ttok) reststr
                        then Just n
                        else ((n+1)+) `fmap` substrPS tok reststr

------------------------------------------------------------------------

-- TODO: replace breakFirstPS and breakLastPS with definitions based on
-- ByteString's break/breakEnd
{-# INLINE breakFirstPS #-}
breakFirstPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakFirstPS c p = case BC.elemIndex c p of
                   Nothing -> Nothing
                   Just n -> Just (B.take n p, B.drop (n+1) p)

{-# INLINE breakLastPS #-}
breakLastPS :: Char -> B.ByteString -> Maybe (B.ByteString,B.ByteString)
breakLastPS c p = case BC.elemIndexEnd c p of
                  Nothing -> Nothing
                  Just n -> Just (B.take n p, B.drop (n+1) p)

-- TODO: rename
{-# INLINE linesPS #-}
linesPS :: B.ByteString -> [B.ByteString]
linesPS ps
     | B.null ps = [B.empty]
     | otherwise = BC.split '\n' ps

{- QuickCheck property:

import Test.QuickCheck
import qualified Data.ByteString.Char8 as BC
import Data.Char
instance Arbitrary BC.ByteString where
    arbitrary = fmap BC.pack arbitrary
instance Arbitrary Char where
  arbitrary = chr `fmap` choose (32,127)
deepCheck = check (defaultConfig { configMaxTest = 10000})
testLines =  deepCheck (\x -> (linesPS x == linesPSOld x))
linesPSOld ps = case  BC.elemIndex '\n' ps of
             Nothing -> [ps]
             Just n -> B.take n ps : linesPS (B.drop (n+1) ps) -}

{-| This function acts exactly like the "Prelude" unlines function, or like
"Data.ByteString.Char8" 'unlines', but with one important difference: it will
produce a string which may not end with a newline! That is:

> unlinesPS ["foo", "bar"]

evaluates to \"foo\\nbar\", not \"foo\\nbar\\n\"! This point should hold true for
'linesPS' as well.

TODO: rename this function. -}
unlinesPS :: [B.ByteString] -> B.ByteString
unlinesPS [] = BC.empty
unlinesPS x  = BC.init $ BC.unlines x
{-# INLINE unlinesPS #-}
{- QuickCheck property:

testUnlines = deepCheck (\x -> (unlinesPS x == unlinesPSOld x))
unlinesPSOld ss = BC.concat $ intersperse_newlines ss
    where intersperse_newlines (a:b:s) = a : newline : intersperse_newlines (b:s)
          intersperse_newlines s = s
          newline = BC.pack "\n" -}

-- -----------------------------------------------------------------------------
-- gzReadFilePS

-- |Decompress the given bytestring into a lazy list of chunks, along with a boolean
-- flag indicating (if True) that the CRC was corrupted.
-- Inspecting the flag will cause the entire list of chunks to be evaluated (but if
-- you throw away the list immediately this should run in constant space).
gzDecompress :: Maybe Int -> BL.ByteString -> ([B.ByteString], Bool)
gzDecompress mbufsize =
    -- This is what the code would be without the bad CRC recovery logic:
    -- return . BL.toChunks . GZ.decompressWith decompressParams
    toListWarn . ZI.decompressWithErrors ZI.gzipFormat decompressParams
  where
        decompressParams = case mbufsize of
                              Just bufsize -> GZ.defaultDecompressParams { GZ.decompressBufferSize = bufsize }
                              Nothing -> GZ.defaultDecompressParams

        toListWarn :: ZI.DecompressStream -> ([B.ByteString], Bool)
        toListWarn = foldDecompressStream (\x ~(xs, b) -> (x:xs, b)) ([], False) handleBad

        -- cut and paste from Zlib since it's not currently exported (interface not yet certain)
        foldDecompressStream :: (B.ByteString -> a -> a) -> a
                             -> (ZI.DecompressError -> String -> a)
                             -> ZI.DecompressStream -> a
        foldDecompressStream chunk end err = fold
                   where
                       fold ZI.StreamEnd               = end
                       fold (ZI.StreamChunk bs stream) = chunk bs (fold stream)
                       fold (ZI.StreamError code msg)  = err code msg

        -- For a while a bug in darcs caused gzip files with good data but bad CRCs to be
        -- produced. Trap bad CRC messages, run the specified action to report that it happened,
        -- but continue on the assumption that the data is valid.
        handleBad ZI.DataError "incorrect data check" = ([], True)
        handleBad _ msg = error msg

isGZFile :: FilePath -> IO (Maybe Int)
isGZFile f = do
    h <- openBinaryFile f ReadMode
    header <- B.hGet h 2
    if header /= BC.pack "\31\139"
       then do hClose h
               return Nothing
       else do hSeek h SeekFromEnd (-4)
               len <- hGetLittleEndInt h
               hClose h
               return (Just len)

-- | Read an entire file, which may or may not be gzip compressed, directly
-- into a 'B.ByteString'.
gzReadFilePS :: FilePath -> IO B.ByteString
gzReadFilePS f = do
    mlen <- isGZFile f
    case mlen of
       Nothing -> mmapFilePS f
       Just len ->
            do -- Passing the length to gzDecompress means that it produces produces one chunk,
               -- which in turn means that B.concat won't need to copy data.
               -- If the length is wrong this will just affect efficiency, not correctness
               let doDecompress buf = let (res, bad) = gzDecompress (Just len) buf
                                      in do when bad $ addCRCWarning f
                                            return res
               compressed <- (BL.fromChunks . return) `fmap` mmapFilePS f
               B.concat `fmap` doDecompress compressed

hGetLittleEndInt :: Handle -> IO Int
hGetLittleEndInt h = do
    b1 <- ord `fmap` hGetChar h
    b2 <- ord `fmap` hGetChar h
    b3 <- ord `fmap` hGetChar h
    b4 <- ord `fmap` hGetChar h
    return $ b1 + 256*b2 + 65536*b3 + 16777216*b4

gzWriteFilePS :: FilePath -> B.ByteString -> IO ()
gzWriteFilePS f ps = gzWriteFilePSs f [ps]

gzWriteFilePSs :: FilePath -> [B.ByteString] -> IO ()
gzWriteFilePSs f pss  =
    BL.writeFile f $ GZ.compress $ BL.fromChunks pss

-- | Read standard input, which may or may not be gzip compressed, directly
-- into a 'B.ByteString'.
gzReadStdin :: IO B.ByteString
gzReadStdin = do header <- B.hGet stdin 2
                 rest   <- B.hGetContents stdin
                 let allStdin = B.concat [header,rest]
                 return $
                  if header /= BC.pack "\31\139"
                   then allStdin
                   else let decompress = fst . gzDecompress Nothing
                            compressed = BL.fromChunks [allStdin]
                        in
                        B.concat $ decompress compressed

-- -----------------------------------------------------------------------------
-- mmapFilePS

-- | Like readFilePS, this reads an entire file directly into a
-- 'B.ByteString', but it is even more efficient.  It involves directly
-- mapping the file to memory.  This has the advantage that the contents of
-- the file never need to be copied.  Also, under memory pressure the page
-- may simply be discarded, wile in the case of readFilePS it would need to
-- be written to swap.  If you read many small files, mmapFilePS will be
-- less memory-efficient than readFilePS, since each mmapFilePS takes up a
-- separate page of memory.  Also, you can run into bus errors if the file
-- is modified.  NOTE: as with 'readFilePS', the string representation in
-- the file is assumed to be ISO-8859-1.

mmapFilePS :: FilePath -> IO B.ByteString
#ifdef HAVE_MMAP
mmapFilePS f = do
  x <- mmapFileByteString f Nothing
   `catch` (\(_ :: SomeException) -> do
                     size <- fileSize `fmap` getSymbolicLinkStatus f
                     if size == 0
                        then return B.empty
                        else performGC >> mmapFileByteString f Nothing)
  return x
#else
mmapFilePS = B.readFile
#endif

-- -------------------------------------------------------------------------
-- fromPS2Hex

foreign import ccall unsafe "static fpstring.h conv_to_hex" conv_to_hex
    :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()

fromPS2Hex :: B.ByteString -> B.ByteString
fromPS2Hex ps = case BI.toForeignPtr ps of
          (x,s,l) ->
           BI.unsafeCreate (2*l) $ \p -> withForeignPtr x $ \f ->
           conv_to_hex p (f `plusPtr` s) $ fromIntegral l

-- -------------------------------------------------------------------------
-- fromHex2PS

foreign import ccall unsafe "static fpstring.h conv_from_hex" conv_from_hex
    :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()

fromHex2PS :: B.ByteString -> B.ByteString
fromHex2PS ps = case BI.toForeignPtr ps of
          (x,s,l) ->
           BI.unsafeCreate (l `div` 2) $ \p -> withForeignPtr x $ \f ->
           conv_from_hex p (f `plusPtr` s) (fromIntegral $ l `div` 2)

-- -------------------------------------------------------------------------
-- betweenLinesPS

-- | betweenLinesPS returns the B.ByteString between the two lines given,
-- or Nothing if they do not appear.

betweenLinesPS :: B.ByteString -> B.ByteString -> B.ByteString
               -> Maybe (B.ByteString)
betweenLinesPS start end ps
 = case break (start ==) (linesPS ps) of
       (_, _:rest@(bs1:_)) ->
           case BI.toForeignPtr bs1 of
            (ps1,s1,_) ->
             case break (end ==) rest of
               (_, bs2:_) -> case BI.toForeignPtr bs2 of (_,s2,_) -> Just $ fromForeignPtr ps1 s1 (s2 - s1)
               _ -> Nothing
       _ -> Nothing

-- -------------------------------------------------------------------------
-- breakAfterNthNewline

breakAfterNthNewline :: Int -> B.ByteString
                        -> Maybe (B.ByteString, B.ByteString)
breakAfterNthNewline 0 the_ps | B.null the_ps = Just (B.empty, B.empty)
breakAfterNthNewline n the_ps =
  case BI.toForeignPtr the_ps of
  (fp,the_s,l) ->
   unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit 0 s | s == end = return $ Just (the_ps, B.empty)
          findit _ s | s == end = return Nothing
          findit 0 s = let left_l = s - the_s
                       in return $ Just (fromForeignPtr fp the_s left_l,
                                         fromForeignPtr fp s (l - left_l))
          findit i s = do w <- peekElemOff p s
                          if w == nl then findit (i-1) (s+1)
                                     else findit i (s+1)
          nl = BI.c2w '\n'
          end = the_s + l
      findit n the_s

-- -------------------------------------------------------------------------
-- breakBeforeNthNewline

breakBeforeNthNewline :: Int -> B.ByteString -> (B.ByteString, B.ByteString)
breakBeforeNthNewline 0 the_ps
 | B.null the_ps = (B.empty, B.empty)
breakBeforeNthNewline n the_ps =
 case BI.toForeignPtr the_ps of
 (fp,the_s,l) ->
   unsafePerformIO $ withForeignPtr fp $ \p ->
   do let findit _ s | s == end = return (the_ps, B.empty)
          findit i s = do w <- peekElemOff p s
                          if w == nl
                            then if i == 0
                                 then let left_l = s - the_s
                                      in return (fromForeignPtr fp the_s left_l,
                                                 fromForeignPtr fp s (l - left_l))
                                 else findit (i-1) (s+1)
                            else findit i (s+1)
          nl = BI.c2w '\n'
          end = the_s + l
      findit n the_s

-- | Decode a ByteString to a String according to the current locale
-- unsafePerformIO in the locale function is ratified by the fact that GHC 6.12
-- and above also supply locale conversion with functions with a pure type.
-- Unrecognized byte sequences in the input are skipped.
decodeLocale :: B.ByteString -> String
decodeLocale = unsafePerformIO . runInputT defaultSettings . decode

-- | Encode a String to a ByteString with latin1 (i.e., the values of the
-- characters become the values of the bytes; if a character value is greater
-- than 255, its byte becomes the character value modulo 256)
encodeLatin1 :: String -> B.ByteString
encodeLatin1 = B.pack . (map (fromIntegral . ord))

-- | Encode a String to a ByteString according to the current locale
encodeLocale :: String -> B.ByteString
encodeLocale = unsafePerformIO . runInputT defaultSettings . encode

-- | Take a @String@ that represents byte values and re-decode it acording to
-- the current locale.
decodeString :: String -> String
decodeString = decodeLocale . encodeLatin1


