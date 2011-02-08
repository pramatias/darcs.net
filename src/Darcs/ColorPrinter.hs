{-# OPTIONS -fno-warn-orphans #-}
module Darcs.ColorPrinter ( errorDoc, traceDoc, assertDoc, fancyPrinters ) where

import Debug.Trace ( trace )
import System.IO ( stderr )
import Darcs.External (getTermNColors)
import Printer (Printer, Printers, Printers'(..), Printable(..), Color(..),
                invisiblePrinter, (<>), (<?>), Doc(Doc,unDoc), unsafeBothText, simplePrinter, hcat,
                unsafeText, unsafeChar, space, unsafePackedString,
                renderStringWith, prefix )
import Data.Char ( isAscii, isPrint, isSpace, isControl, ord, chr )
import Data.Bits ( bit, xor )
import System.Environment ( getEnv )
import qualified Data.ByteString.Char8 as BC (unpack, any, last, spanEnd)
import qualified Data.ByteString       as B (null, init)
import System.IO.Unsafe ( unsafePerformIO )
import System.IO ( hIsTerminalDevice, Handle )
import Text.Printf ( printf )

dollar, cr :: Doc
dollar = unsafeBothText "$"
cr     = unsafeBothText "\r"

errorDoc :: Doc -> a
errorDoc = error . show

traceDoc :: Doc -> a -> a
traceDoc d = trace (show d)

assertDoc :: Maybe Doc -> a -> a
assertDoc Nothing x = x
assertDoc (Just e) _ = errorDoc e

instance Show Doc where
    show = renderStringWith (fancyPrinters stderr)

-- policy
-- | the 'Policy' type is a record containing the variables which control
-- how 'Doc's will be rendered on some output.
data Policy = Policy { poColor :: Bool    -- ^ overall use of color
                     , poEscape :: Bool   -- ^ overall use of escaping
                     , poLineColor :: Bool -- ^ overall use of colored lines (only hunks for now)
                     , poAltColor :: Bool -- ^ alternative to color (bold, inverse)
                     , poIsprint :: Bool  -- ^ don't escape isprints
                     , po8bit  :: Bool    -- ^ don't escape 8-bit chars
                     , poNoEscX :: String   -- ^ extra chars to never escape
                     , poEscX :: String   -- ^ extra chars to always escape
                     , poTrailing :: Bool -- ^ escape trailing spaces
                     , poCR :: Bool       -- ^ ignore \r at end of lines
                     , poSpace :: Bool    -- ^ escape spaces (used with poTrailing)
                     }

{-# NOINLINE getPolicy #-}
-- | 'getPolicy' returns a suitable policy for a given handle.
-- The policy is chosen according to environment variables, and to the
-- type of terminal which the handle represents
getPolicy :: Handle -> Policy
getPolicy handle = unsafePerformIO $
 do isTerminal <- hIsTerminalDevice handle
    nColors <- if isTerminal then getTermNColors else return 0

    envDontEscapeAnything  <- getEnvBool "DARCS_DONT_ESCAPE_ANYTHING"
    envDontEscapeIsprint   <- getEnvBool "DARCS_DONT_ESCAPE_ISPRINT"
    envUseIsprint          <- getEnvBool "DARCS_USE_ISPRINT" -- for backwards-compatibility
    envDontEscape8bit      <- getEnvBool "DARCS_DONT_ESCAPE_8BIT"

    envDontEscapeExtra  <- getEnvString "DARCS_DONT_ESCAPE_EXTRA"
    envEscapeExtra      <- getEnvString "DARCS_ESCAPE_EXTRA"

    envDontEscapeTrailingSpace  <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_SPACES"
    envDontEscapeTrailingCR     <- getEnvBool "DARCS_DONT_ESCAPE_TRAILING_CR"

    envDontColor         <- getEnvBool "DARCS_DONT_COLOR"
    envAlwaysColor       <- getEnvBool "DARCS_ALWAYS_COLOR"
    envAlternativeColor  <- getEnvBool "DARCS_ALTERNATIVE_COLOR"
    envDoColorLines    <- getEnvBool "DARCS_DO_COLOR_LINES"

    let haveColor = envAlwaysColor || (isTerminal && (nColors > 4))
        doColor   = not envDontColor && haveColor

    return Policy { poColor    = doColor,
                    poEscape   = not envDontEscapeAnything,
                    poLineColor= doColor && envDoColorLines,
                    poIsprint  = envDontEscapeIsprint || envUseIsprint,
                    po8bit     = envDontEscape8bit,
                    poNoEscX   = envDontEscapeExtra,
                    poEscX     = envEscapeExtra,
                    poTrailing = not envDontEscapeTrailingSpace,
                    poCR       = envDontEscapeTrailingCR,
                    poAltColor = haveColor && envAlternativeColor,

                    poSpace = False
                  }
 where
  getEnvBool s = safeGetEnv s >>= return.(/= "0")
  safeGetEnv s = getEnv s `catch` \_ -> return "0"
  getEnvString s = getEnv s `catch` \_ -> return ""


-- printers

-- | @'fancyPrinters' h@ returns a set of printers suitable for outputting
-- to @h@
fancyPrinters :: Printers
fancyPrinters h = let policy = getPolicy h in
                      Printers { colorP = colorPrinter policy,
                             invisibleP = invisiblePrinter,
                             hiddenP = colorPrinter policy Green,
                             userchunkP = userchunkPrinter policy,
                             defP       = escapePrinter policy,
                             lineColorT = lineColorTrans policy,
                             lineColorS = lineColorSuffix policy
                           }

-- | @'lineColorTrans' policy@ tries to color a Doc, according to policy po.
-- That is, if @policy@ has @poLineColor@ set, then colors the line, otherwise
-- does nothing.
lineColorTrans :: Policy -> Color -> Doc -> Doc
lineColorTrans po | poLineColor po = \c d -> prefix (setColor c) d <?> unsafeBothText resetColor
                  | otherwise      = const id

lineColorSuffix :: Policy -> [Printable] -> [Printable]
lineColorSuffix po | poLineColor po = \d -> S resetColor : d
                   | otherwise      = id

colorPrinter :: Policy -> Color -> Printer
colorPrinter po | poColor po = \c -> unDoc . color po c . Doc . escapePrinter po{poColor=False}
                | otherwise  = const $ escapePrinter po

userchunkPrinter :: Policy -> Printer
userchunkPrinter po p
 | not (poEscape po)   = simplePrinter p
 | not (poTrailing po) = escapePrinter po p
 | otherwise           = unDoc $ pr p
 where
  pr (S s)       = prString s
  pr (Both _ ps) = prPS ps
  pr (PS ps)     = prPS ps

  prPS ps = let (leadPS, trailPS) = BC.spanEnd isSpace ps
            in if B.null trailPS
                then Doc $ escapePrinter po p
                else Doc (escapePrinter po (PS leadPS))
                  <> Doc (escapePrinter po{poSpace=True} (PS trailPS))
                  <> markEscape po dollar

  prString s = let (trail',lead') = span isSpace (reverse s)
                   lead = reverse lead'
                   trail = reverse trail'
               in if (not.null) trail
                   then Doc (escapePrinter po (S lead))
                     <> Doc (escapePrinter po{poSpace=True} (S trail))
                     <> markEscape po dollar
                   else Doc (escapePrinter po p)

escapePrinter :: Policy -> Printer
escapePrinter po
 | (not.poEscape) po = simplePrinter
 | otherwise         = unDoc . crepr
 where
  crepr p | poCR po && isEndCR p = epr (initPR p) <> cr
          | otherwise            = epr p

  epr (S s)      = escape po s
  epr (PS ps)    = if BC.any (not.noEscape po) ps
                   then escape po (BC.unpack ps)
                   else unsafePackedString ps
  epr (Both s _) = escape po s

  isEndCR (S s)        = not (null s) && last s == '\r'
  isEndCR (PS ps)      = not (B.null ps) && BC.last ps == '\r'
  isEndCR (Both _ ps)  = not (B.null ps) && BC.last ps == '\r'

  initPR (S s)       = S $ init s
  initPR (PS ps)     = PS $ B.init ps
  initPR (Both s ps) = Both (init s) (B.init ps)


-- | @'escape' policy string@ escapes @string@ according to the rules
-- defined in 'policy', turning it into a 'Doc'.
escape :: Policy -> String -> Doc
escape _ "" = unsafeText ""
escape po s = hcat (map escapeChar s)
 where
  escapeChar c | noEscape po c = unsafeChar c
  escapeChar ' ' = space
  escapeChar c = (emph.unsafeText.quoteChar) c
  emph = markEscape po

-- | @'noEscape' policy c@ tells wether @c@ will be left as-is
-- when escaping according to @policy@
noEscape :: Policy -> Char -> Bool
noEscape po c | poSpace po && isSpace c = False
noEscape po c | c `elem` poEscX po = False
noEscape po c | c `elem` poNoEscX po = True
noEscape _ '\t' = True  -- tabs will likely be converted to spaces
noEscape _ '\n' = True
noEscape po c = if (poIsprint po) then isPrint c
                                   else isPrintableAscii c
                 ||  c >= '\x80' && po8bit po

-- | 'isPrintableAscii' tells wether a character is a printable character
-- of the ascii range.
isPrintableAscii :: Char -> Bool
isPrintableAscii c = isAscii c && isPrint c


-- | 'quoteChar' represents a special character as a string.
--   * @quoteChar '^c'@ (where @^c@ is a control character) is @"^c"@
--   * Otherwise, @quoteChar@ returns "\hex", where 'hex' is the
--     hexadecimal number of the character.
quoteChar :: Char -> String
quoteChar c
 | isControl c && isPrintableAscii cHat = ['^', cHat]
 | otherwise = sHex
 where
  cHat = chr $ (bit 6 `xor`) $ ord c
  sHex = "<U+" ++ printf "%04X" c ++ ">"


-- make colors and highlightings

-- | @'markEscape' policy doc@ marks @doc@ with the appropriate
-- marking for escaped characters according to @policy@
markEscape :: Policy -> Doc -> Doc
markEscape po  | poAltColor po  = makeInvert
               | poColor po     = makeColor Red
               | otherwise      = makeAsciiart

-- | @'color' policy color doc@ colors @doc@ with color @color@ if
-- @policy@ is not set to use an alternative to color. In that case,
-- it makes the text bold instead.
color :: Policy -> Color -> Doc -> Doc
color po | poAltColor po = \_ -> makeBold
         | otherwise     = makeColor

makeColor, makeColor' :: Color -> Doc -> Doc

makeColor' = withColor . setColor

-- memoized version of makeColor'
makeColor Blue    = makeColor' Blue
makeColor Red     = makeColor' Red
makeColor Green   = makeColor' Green
makeColor Cyan    = makeColor' Cyan
makeColor Magenta = makeColor' Magenta

setColor :: Color -> String
setColor Blue    = "\x1B[01;34m" -- bold blue
setColor Red     = "\x1B[01;31m" -- bold red
setColor Green   = "\x1B[01;32m" -- bold green
setColor Cyan    = "\x1B[36m"    -- light cyan
setColor Magenta = "\x1B[35m"    -- light magenta

-- | @'makeAsciiart' doc@ tries to make @doc@ (usually a
-- single escaped char) stand out with the help of only plain
-- ascii, i.e., no color or font style.
makeAsciiart :: Doc -> Doc
makeAsciiart x = unsafeBothText "[_" <> x <> unsafeBothText "_]"

-- | the string to reset the terminal's color.
resetColor :: String
resetColor = "\x1B[00m"

-- | @'withColor' color doc@ returns a colorized version of @doc@.
-- @color@ is a string that represents a color, given by 'setColor'
withColor :: String -> Doc -> Doc
withColor c =
   let c' = unsafeBothText c
       r' = unsafeBothText resetColor
   in \x -> c' <> x <> r'


-- | 'makeBold' boldens a doc.
makeBold :: Doc -> Doc
-- | 'makeInvert' returns an invert video version of a doc.
makeInvert :: Doc -> Doc
makeBold   = withColor "\x1B[01m"
makeInvert = withColor "\x1B[07m"
