-- Copyright (C) 2003 Peter Simons
-- Copyright (C) 2003,2008 David Roundy
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

-- This module is intended to provide backwards-compatibility in the parsing
-- of darcs patches.  In other words: don't change it, new features don't get
-- added here.  The only user should be Darcs.Patch.Info.

module Darcs.Patch.OldDate ( readUTCDate, showIsoDateTime ) where

import Text.ParserCombinators.Parsec
import System.Time
import Data.Char ( toUpper, isDigit )
import Control.Monad ( liftM, liftM2 )
import qualified Data.ByteString.Char8 as B
import Data.Maybe ( fromMaybe )

-- | Read/interpret a date string, assuming UTC if timezone
--   is not specified in the string
readUTCDate :: String -> CalendarTime
readUTCDate = readDate 0

readDate :: Int -> String -> CalendarTime
readDate tz d =
             case parseDate tz d of
             Left e -> error e
             Right ct -> ct

parseDate :: Int -> String -> Either String CalendarTime
parseDate tz d =
              if length d >= 14 && B.all isDigit bd
              then Right $
                   CalendarTime (readI $ B.take 4 bd)
                                (toEnum $ (+ (-1)) $ readI $ B.take 2 $ B.drop 4 bd)
                                (readI $ B.take 2 $ B.drop 6 bd) -- Day
                                (readI $ B.take 2 $ B.drop 8 bd) -- Hour
                                (readI $ B.take 2 $ B.drop 10 bd) -- Minute
                                (readI $ B.take 2 $ B.drop 12 bd) -- Second
                                0 Sunday 0 -- Picosecond, weekday and day of year unknown
                                "GMT" 0 False
              else let dt = do { x <- dateTime tz; eof; return x }
                   in case parse dt "" d of
                      Left e -> Left $ "bad date: "++d++" - "++show e
                      Right ct -> Right ct
  where bd = B.pack (take 14 d)
        readI s = fst $ fromMaybe (error "parseDate: invalid date") (B.readInt s)

showIsoDateTime :: CalendarTime -> String
showIsoDateTime ct = concat [ show $ ctYear ct
                            , twoDigit . show . (+1) . fromEnum $ ctMonth ct
                            , twoDigit . show $ ctDay ct
                            , twoDigit . show $ ctHour ct
                            , twoDigit . show $ ctMin ct
                            , twoDigit . show $ ctSec ct
                            ]
    where twoDigit []          = undefined
          twoDigit x@(_:[])    = '0' : x
          twoDigit x@(_:_:[])  = x
          twoDigit _           = undefined

----- Parser Combinators ---------------------------------------------

-- |Case-insensitive variant of Parsec's 'char' function.

caseChar        :: Char -> GenParser Char a Char
caseChar c       = satisfy (\x -> toUpper x == toUpper c)

-- |Case-insensitive variant of Parsec's 'string' function.

caseString      :: String -> GenParser Char a ()
caseString cs    = mapM_ caseChar cs <?> cs

-- |Match a parser at least @n@ times.

manyN           :: Int -> GenParser a b c -> GenParser a b [c]
manyN n p
    | n <= 0     = return []
    | otherwise  = liftM2 (++) (count n p) (many p)

-- |Match a parser at least @n@ times, but no more than @m@ times.

manyNtoM        :: Int -> Int -> GenParser a b c -> GenParser a b [c]
manyNtoM n m p
    | n < 0      = return []
    | n > m      = return []
    | n == m     = count n p
    | n == 0     = foldr (<|>) (return []) (map (\x -> try $ count x p) (reverse [1..m]))
    | otherwise  = liftM2 (++) (count n p) (manyNtoM 0 (m-n) p)


----- Date/Time Parser -----------------------------------------------

dateTime :: Int -> CharParser a CalendarTime
dateTime tz =
            choice [try $ cvsDateTime tz,
                    try $ iso8601DateTime tz,
                    oldDateTime]

cvsDateTime :: Int -> CharParser a CalendarTime
cvsDateTime tz =
                do y <- year
                   _ <- char '/'
                   mon <- monthNum
                   _ <- char '/'
                   d <- day
                   _ <- mySpaces
                   h <- hour
                   _ <- char ':'
                   m <- minute
                   _ <- char ':'
                   s <- second
                   z <- option tz $ mySpaces >> zone
                   return (CalendarTime y mon d h m s 0 Monday 0 "" z False)

oldDateTime   :: CharParser a CalendarTime
oldDateTime    =   do wd <- dayName
                      _ <- mySpaces
                      mon <- monthName
                      _ <- mySpaces
                      d <- day
                      _ <- mySpaces
                      h <- hour
                      _ <- char ':'
                      m <- minute
                      _ <- char ':'
                      s <- second
                      _ <- mySpaces
                      z <- zone
                      _ <- mySpaces
                      y <- year
                      return (CalendarTime y mon d h m s 0 wd 0 "" z False)

{- FIXME: In case you ever want to use this outside of darcs, you should note
   that this implementation of ISO 8601 is not complete.

   reluctant to implement (ambiguous!):
     * years > 9999
     * truncated representations with implied century (89 for 1989)
   unimplemented:
     * repeated durations (not relevant)
     * lowest order component fractions in intervals
     * negative dates (BC)
   unverified or too relaxed:
     * the difference between 24h and 0h
     * allows stuff like 2005-1212; either you use the hyphen all the way
       (2005-12-12) or you don't use it at all (20051212), but you don't use
       it halfway, likewise with time
     * No bounds checking whatsoever on intervals!
       (next action: read iso doc to see if bounds-checking required?) -}
iso8601DateTime   :: Int -> CharParser a CalendarTime
iso8601DateTime localTz = try $
  do d <- iso8601Date
     t <- option id $ try $ do optional $ oneOf " T"
                               iso8601Time
     return $ t $ d { ctTZ = localTz }

iso8601Date :: CharParser a CalendarTime
iso8601Date =
  do d <- calendar_date <|> week_date <|> ordinal_date
     return $ foldr ($) nullCalendar d
  where
    calendar_date = -- yyyy-mm-dd
      try $ do d <- optchain year_ [ (dash, month_), (dash, day_) ]
               -- allow other variants to be parsed correctly
               notFollowedBy (digit <|> char 'W')
               return d
    week_date = --yyyy-Www-dd
      try $ do yfn <- year_
               optional dash
               _ <- char 'W'
               -- offset human 'week 1' -> computer 'week 0'
               w'  <- (\x -> x-1) `liftM` twoDigits
               wd  <- option 1 $ do { optional dash; nDigits 1 }
               let y = yfn nullCalendar
                   firstDay = ctWDay y
               -- things that make this complicated
               -- 1. iso8601 weeks start from Monday; Haskell weeks start from Sunday
               -- 2. the first week is the one that contains at least Thursday
               --    if the year starts after Thursday, then some days of the year
               --    will have already passed before the first week
               let afterThursday = firstDay == Sunday || firstDay > Thursday
                   w  = if afterThursday then w'+1 else w'
                   diff c = c { ctDay = (7 * w) + wd - (fromEnum firstDay) }
               return [(toUTCTime.toClockTime.diff.yfn)]
    ordinal_date = -- yyyy-ddd
      try $ optchain year_ [ (dash, yearDay_) ]
    --
    year_  = try $ do y <- fourDigits <?> "year (0000-9999)"
                      return $ \c -> c { ctYear = y }
    month_ = try $ do m <- twoDigits <?> "month (1 to 12)"
                      -- we (artificially) use ctPicosec to indicate
                      -- whether the month has been specified.
                      return $ \c -> c { ctMonth = intToMonth m, ctPicosec = 0 }
    day_   = try $ do d <- twoDigits <?> "day in month (1 to 31)"
                      return $ \c -> c { ctDay = d }
    yearDay_ = try $ do d <- nDigits 3 <?> "day in year (1 to 366)"
                        return $ \c -> c { ctYDay = d }
    dash = char '-'

-- we return a function which sets the time on another calendar
iso8601Time :: CharParser a (CalendarTime -> CalendarTime)
iso8601Time = try $
  do ts <- optchain hour_ [ (colon     , min_)
                          , (colon     , sec_)
                          , (oneOf ",.", pico_) ]
     z  <- option id $ choice [ zulu , offset ]
     return $ foldr (.) id (z:ts)
  where
    hour_ = do h <- twoDigits
               return $ \c -> c { ctHour = h }
    min_  = do m <- twoDigits
               return $ \c -> c { ctMin = m }
    sec_  = do s <- twoDigits
               return $ \c -> c { ctSec = s }
    pico_ = do digs <- many digit
               let picoExp = 12
                   digsExp = length digs
               let frac | null digs = 0
                        | digsExp > picoExp = read $ take picoExp digs
                        | otherwise = 10 ^ (picoExp - digsExp) * (read digs)
               return $ \c -> c { ctPicosec = frac }
    zulu   = do { _ <- char 'Z'; return (\c -> c { ctTZ = 0 }) }
    offset = do sign <- choice [ do { char '+' >> return   1  }
                               , do { char '-' >> return (-1) } ]
                h <- twoDigits
                m <- option 0 $ do { optional colon; twoDigits }
                return $ \c -> c { ctTZ = sign * 60 * ((h*60)+m) }
    colon = char ':'

optchain :: CharParser a b -> [(CharParser a c, CharParser a b)] -> CharParser a [b]
optchain p next = try $
  do r1 <- p
     r2 <- case next of
           [] -> return []
           ((sep,p2):next2) -> option [] $ do { optional sep; optchain p2 next2 }
     return (r1:r2)

nDigits :: Int -> CharParser a Int
nDigits n = read `liftM` count n digit

twoDigits, fourDigits :: CharParser a Int
twoDigits = nDigits 2
fourDigits = nDigits 4

mySpaces :: CharParser a String
mySpaces = manyN 1 $ char ' '

dayName        :: CharParser a Day
dayName         = choice
                       [ caseString "Mon"       >> return Monday
                       , try (caseString "Tue") >> return Tuesday
                       , caseString "Wed"       >> return Wednesday
                       , caseString "Thu"       >> return Thursday
                       , caseString "Fri"       >> return Friday
                       , try (caseString "Sat") >> return Saturday
                       , caseString "Sun"       >> return Sunday
                       ]

year            :: CharParser a Int
year             = fourDigits

monthNum       :: CharParser a Month
monthNum = do mn <- manyNtoM 1 2 digit
              return $ intToMonth $ (read mn :: Int)

intToMonth :: Int -> Month
intToMonth 1 = January
intToMonth 2 = February
intToMonth 3 = March
intToMonth 4 = April
intToMonth 5 = May
intToMonth 6 = June
intToMonth 7 = July
intToMonth 8 = August
intToMonth 9 = September
intToMonth 10 = October
intToMonth 11 = November
intToMonth 12 = December
intToMonth _  = error "invalid month!"

monthName      :: CharParser a Month
monthName       = choice
                       [ try (caseString "Jan") >> return January
                       , caseString "Feb"       >> return February
                       , try (caseString "Mar") >> return March
                       , try (caseString "Apr") >> return April
                       , caseString "May"       >> return May
                       , try (caseString "Jun") >> return June
                       , caseString "Jul"       >> return July
                       , caseString "Aug"       >> return August
                       , caseString "Sep"       >> return September
                       , caseString "Oct"       >> return October
                       , caseString "Nov"       >> return November
                       , caseString "Dec"       >> return December
                       ]

day             :: CharParser a Int
day              = do d <- manyNtoM 1 2 digit
                      return (read d :: Int)

hour            :: CharParser a Int
hour             = twoDigits

minute          :: CharParser a Int
minute           = twoDigits

second          :: CharParser a Int
second           = twoDigits

zone            :: CharParser a Int
zone             = choice
                       [ do { _ <- char '+'; h <- hour; m <- minute; return (((h*60)+m)*60) }
                       , do { _ <- char '-'; h <- hour; m <- minute; return (-((h*60)+m)*60) }
                       , mkZone "UTC"  0
                       , mkZone "UT"  0
                       , mkZone "GMT" 0
                       , mkZone "EST" (-5)
                       , mkZone "EDT" (-4)
                       , mkZone "CST" (-6)
                       , mkZone "CDT" (-5)
                       , mkZone "MST" (-7)
                       , mkZone "MDT" (-6)
                       , mkZone "PST" (-8)
                       , mkZone "PDT" (-7)
                       , mkZone "CEST" 2
                       , mkZone "EEST" 3
                         -- if we don't understand it, just give a GMT answer...
                       , do { _ <- manyTill (oneOf $ ['a'..'z']++['A'..'Z']++[' '])
                                       (lookAhead space_digit);
                              return 0 }
                       ]
     where mkZone n o  = try $ do { caseString n; return (o*60*60) }
           space_digit = try $ do { _ <- char ' '; oneOf ['0'..'9'] }

nullCalendar :: CalendarTime
nullCalendar = CalendarTime 0 January 0 0 0 0 1 Sunday 0 "" 0 False
