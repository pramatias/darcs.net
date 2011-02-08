{-# LANGUAGE CPP #-}
module Darcs.Email ( makeEmail, readEmail, formatHeader ) where

import Data.Char ( digitToInt, isHexDigit, ord, intToDigit, isPrint, toUpper )
import Data.List ( isInfixOf )
import Printer ( Doc, ($$), (<+>), (<>), text, empty, packedString, renderPS)

import ByteStringUtils ( packStringToUTF8, dropSpace, linesPS, betweenLinesPS )
import qualified Data.ByteString          as B  (ByteString, length, null, tail
                                                ,drop, head, concat, singleton
                                                ,pack, append, empty, unpack
                                                )
import qualified Data.ByteString.Char8    as BC (index, head, pack)
import Data.ByteString.Internal as B (c2w, createAndTrim)
import System.IO.Unsafe ( unsafePerformIO )
import Foreign.Ptr ( Ptr, plusPtr )
import Foreign.Storable ( poke )
import Data.Word ( Word8 )

-- lineMax is maximum number of characters in an e-mail line excluding the CRLF
-- at the end. qlineMax is the number of characters in a q-encoded or
-- quoted-printable-encoded line.
lineMax, qlineMax :: Int
lineMax  = 78
qlineMax = 75

-- | Formats an e-mail header by encoding any non-ascii characters using UTF-8
--   and Q-encoding, and folding lines at appropriate points. It doesn't do
--   more than that, so the header name and header value should be
--   well-formatted give or take line length and encoding. So no non-ASCII
--   characters within quoted-string, quoted-pair, or atom; no semantically
--   meaningful signs in names; no non-ASCII characters in the header name;
--   etcetera.
formatHeader :: String -> String -> B.ByteString
formatHeader headerName headerValue =
    B.append nameColon encodedValue
  where nameColon = B.pack (map B.c2w (headerName ++ ":")) -- space for folding
        encodedValue = foldAndEncode (' ':headerValue)
                                       (B.length nameColon) False False

-- run through a string and encode non-ascii words and fold where appropriate.
-- the integer argument is the current position in the current line.
-- the string in the first argument must begin with whitespace, or be empty.
foldAndEncode :: String -> Int -> Bool -> Bool -> B.ByteString
foldAndEncode [] _ _               _         = B.empty
foldAndEncode s  p lastWordEncoded inMidWord =
  let newline  = B.singleton 10
      space    = B.singleton 32
      s2bs     = B.pack . map B.c2w
      -- the twelve there is the max number of ASCII chars to encode a single
      -- character: 4 * 3, 4 UTF-8 bytes times 3 ASCII chars per byte
      safeEncChunkLength = (qlineMax - B.length encodedWordStart
                                      - B.length encodedWordEnd) `div` 12
      (curSpace, afterCurSpace) = span (== ' ') s
      (curWord,  afterCurWord)  = break (== ' ') afterCurSpace
      qEncWord | lastWordEncoded = qEncode (curSpace ++ curWord)
               | otherwise       = qEncode curWord
      mustEncode = inMidWord
                   || any (\c -> not (isPrint c) || (ord c) > 127) curWord
                   || length curWord > lineMax - 1
                   || isInfixOf "=?" curWord
      mustFold
        | mustEncode && lastWordEncoded
            = p + 1 + B.length qEncWord > lineMax
        | mustEncode
            = p + length curSpace + B.length qEncWord > lineMax
        | otherwise
            = p + length curSpace + length curWord > lineMax
      mustSplit = (B.length qEncWord > qlineMax && mustEncode)
                  || length curWord > lineMax - 1
      spaceToInsert | mustEncode && lastWordEncoded = space
                    | otherwise                     = s2bs curSpace
      wordToInsert
        | mustEncode && mustSplit = qEncode (take safeEncChunkLength curWord)
        | mustEncode = qEncWord
        | otherwise  = s2bs curWord
      doneChunk | mustFold  = B.concat [newline, spaceToInsert, wordToInsert]
                | otherwise = B.concat [spaceToInsert, wordToInsert]
      (rest, nextP)
        | mustSplit
            = (drop safeEncChunkLength curWord ++ afterCurWord, qlineMax + 1)
        | mustEncode && mustFold
            = (afterCurWord, B.length spaceToInsert + B.length wordToInsert)
        | otherwise
            = (afterCurWord, p + B.length doneChunk)
  in B.append doneChunk (foldAndEncode rest nextP mustEncode mustSplit)

-- | Turns a piece of string into a q-encoded block
--   Applies q-encoding, for use in e-mail header values, as defined in RFC 2047.
--   It just takes a string and builds an encoded-word from it, it does not check
--   length or necessity.
qEncode :: String -> B.ByteString
qEncode s = B.concat [encodedWordStart,
                      encodedString,
                      encodedWordEnd]
  where encodedString =  B.concat (map qEncodeChar s)

encodedWordStart, encodedWordEnd :: B.ByteString
encodedWordStart = B.pack (map B.c2w "=?UTF-8?Q?")
encodedWordEnd   = B.pack (map B.c2w "?=")

-- turns a character into its q-encoded bytestring value. For most printable
-- ASCII characters, that's just the singleton bytestring with that char.
qEncodeChar :: Char -> B.ByteString
qEncodeChar c
    | c == ' '                          = c2bs '_'
    | isPrint c
      && not (c `elem` ['?', '=', '_'])
      && ord c < 128                    = c2bs c
    | otherwise                         = B.concat
                                            (map qbyte
                                              (B.unpack
                                                (packStringToUTF8 [c])))
  where c2bs = B.singleton . B.c2w
        -- qbyte turns a byte into its q-encoded "=hh" representation
        qbyte b = B.pack (map B.c2w ['='
                                    ,word8ToUDigit (b `div` 16)
                                    ,word8ToUDigit (b `mod` 16)
                                    ])
        word8ToUDigit :: Word8 -> Char
        word8ToUDigit = toUpper . intToDigit . fromIntegral

-- TODO is this doing mime encoding??
qpencode :: B.ByteString -> B.ByteString
qpencode s = unsafePerformIO
           -- Really only (3 + 2/75) * length or something in the worst case
           $ B.createAndTrim (4 * B.length s) (\buf -> encode s qlineMax buf 0)

encode :: B.ByteString -> Int -> Ptr Word8 -> Int -> IO Int
encode ps _ _ bufi | B.null ps = return bufi
encode ps n buf bufi = case B.head ps of
  c | c == newline ->
        do poke (buf `plusPtr` bufi) newline
           encode ps' qlineMax buf (bufi+1)
    | n == 0 && B.length ps > 1 ->
        do poke (buf `plusPtr` bufi) equals
           poke (buf `plusPtr` (bufi+1)) newline
           encode ps qlineMax buf (bufi + 2)
    | (c == tab || c == space) ->
        if B.null ps' || B.head ps' == newline
        then do poke (buf `plusPtr` bufi) c
                poke (buf `plusPtr` (bufi+1)) equals
                poke (buf `plusPtr` (bufi+2)) newline
                encode ps' qlineMax buf (bufi + 3)
        else do poke (buf `plusPtr` bufi) c
                encode ps' (n - 1) buf (bufi + 1)
    | (c >= bang && c /= equals && c <= tilde) ->
        do poke (buf `plusPtr` bufi) c
           encode ps' (n - 1) buf (bufi + 1)
    | n < 3 ->
        encode ps 0 buf bufi
    | otherwise ->
        do let (x, y) = c `divMod` 16
               h1 = intToUDigit x
               h2 = intToUDigit y
           poke (buf `plusPtr` bufi) equals
           poke (buf `plusPtr` (bufi+1)) h1
           poke (buf `plusPtr` (bufi+2)) h2
           encode ps' (n - 3) buf (bufi + 3)
    where ps' = B.tail ps
          newline = B.c2w '\n'
          tab     = B.c2w '\t'
          space   = B.c2w ' '
          bang    = B.c2w '!'
          tilde   = B.c2w '~'
          equals  = B.c2w '='
          intToUDigit i
            | i >= 0  && i <= 9  = B.c2w '0' + i
            | i >= 10 && i <= 15 = B.c2w 'A' + i - 10
            | otherwise = error $ "intToUDigit: '"++show i++"'not a digit"

qpdecode :: B.ByteString -> B.ByteString
qpdecode s = unsafePerformIO
             -- Add 1 as linesPS "\n" -> ["", ""] -> "\n\n"
           $ B.createAndTrim (B.length s + 1) (\buf -> decode (linesPS s) buf 0)

decode :: [B.ByteString] -> Ptr Word8 -> Int -> IO Int
decode [] _ bufi = return bufi
decode (ps:pss) buf bufi
 | B.null (dropSpace ps)
    = do poke (buf `plusPtr` bufi) newline
         decode pss buf (bufi+1)
 | is_equals && B.length ps >= 3 && isHexDigit c1 && isHexDigit c2
    = do poke (buf `plusPtr` bufi)
              (toWord8 $ digitToInt c1 * 16 + digitToInt c2)
         decode (B.drop 3 ps:pss) buf (bufi+1)
 | is_equals && B.null (dropSpace (B.tail ps)) = decode pss buf bufi
 | otherwise = do poke (buf `plusPtr` bufi) (B.head ps)
                  decode (B.tail ps:pss) buf (bufi+1)
    where is_equals = BC.head ps == '='
          c1 = BC.index ps 1
          c2 = BC.index ps 2
          newline = B.c2w '\n'
          toWord8 :: Int -> Word8
          toWord8 = fromIntegral

makeEmail :: String -> [(String, String)] -> (Maybe Doc) -> Doc -> (Maybe String) -> Doc
makeEmail repodir headers mcontents bundle mfilename =
    text "DarcsURL:" <+> text repodir
 $$ foldl (\m (h,v) -> m $$ (text (h ++ ":") <+> text v)) empty headers
 $$ text "MIME-Version: 1.0"
 $$ text "Content-Type: multipart/mixed; boundary=\"=-\""
 $$ text ""
 $$ text "--=-"
 $$ (case mcontents of
       Just contents ->
            text "Content-Type: text/plain"
         $$ text "Content-Transfer-Encoding: quoted-printable"
         $$ text ""
         $$ packedString (qpencode (renderPS contents))
         $$ text ""
         $$ text "--=-"
       Nothing -> empty)
 $$ text "Content-Type: multipart/alternative; boundary=\"=_\""
 $$ text ""
 $$ text "--=_"
 $$ text "Content-Type: text/x-darcs-patch"
 $$ text "Content-Transfer-Encoding: quoted-printable"
 $$ text "Content-Description: Patch preview"
 $$ text ""
 $$ (case betweenLinesPS (BC.pack "New patches:") (BC.pack "Context:") (renderPS bundle) of
     Just s -> packedString $ qpencode s
     -- this should not happen, but in case it does, keep everything
     Nothing -> packedString $ qpencode $ renderPS bundle)
 $$ text "--=_"
 $$ text "Content-Type: application/x-darcs-patch" <>
      (case mfilename of
         Just filename -> text "; name=\"" <> text filename <> text "\""
         Nothing -> empty)
 $$ text "Content-Transfer-Encoding: quoted-printable"
 $$ text "Content-Description: A darcs patch for your repository!"
 $$ text ""
 $$ packedString (qpencode (renderPS bundle))
 $$ text "--=_--"
 $$ text "--=---"
 $$ text ""
 $$ text "."
 $$ text ""
 $$ text ""

readEmail :: B.ByteString -> B.ByteString
readEmail s =
    case betweenLinesPS
         (BC.pack "Content-Description: A darcs patch for your repository!")
         (BC.pack "--=_--") s of
    Nothing -> s -- if it wasn't an email in the first place, just pass along.
    Just s' -> qpdecode s'

