-- Copyright (C) 2003 Peter Simons
-- Copyright (C) 2003 David Roundy
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


module IsoDate ( getIsoDateTime, readLocalDate, readUTCDate,
                 parseDate, getLocalTz,
                 englishDateTime, englishInterval, englishLast,
                 iso8601Interval, iso8601Duration,
                 cleanLocalDate, resetCalendar,
                 MCalendarTime(..), subtractFromMCal, addToMCal,
                 toMCalendarTime, unsafeToCalendarTime,
                 unsetTime, TimeInterval
               ) where

import Text.ParserCombinators.Parsec
import System.Time
import System.IO.Unsafe ( unsafePerformIO )
import Data.Char ( toUpper, isDigit )
import Data.Maybe ( fromMaybe )
import Control.Monad ( liftM, liftM2 )
import qualified Data.ByteString.Char8 as B

type TimeInterval = (Maybe CalendarTime, Maybe CalendarTime)

-- | Read/interpret a date string, assuming UTC if timezone
--   is not specified in the string (see 'readDate')
--   Warning! This errors out if we fail to interpret the
--   date
readUTCDate :: String -> CalendarTime
readUTCDate = readDate 0

-- | Convert a date string into ISO 8601 format (yyyymmdd variant)
--   assuming local timezone if not specified in the string
--   Warning! This errors out if we fail to interpret the date
cleanLocalDate :: String -> String
cleanLocalDate = showIsoDateTime . resetCalendar
                 . readDate (unsafePerformIO getLocalTz)

-- | Read/interpret a date string, assuming local timezone if not
--   specified in the string
readLocalDate :: String -> CalendarTime
readLocalDate = readDate (unsafePerformIO getLocalTz)

-- | Return the local timezone offset from UTC in seconds
getLocalTz :: IO Int
getLocalTz = ctTZ `liftM` (getClockTime >>= toCalendarTime)

-- | Parse a date string with 'parseDate'
--   Warning! This errors out if we fail to interpret the date
--   Uses its first argument as the default time zone.
readDate :: Int -> String -> CalendarTime
readDate tz d =
             case parseDate tz d of
             Left e -> error $ "bad date: "++d++" - "++show e
             Right ct -> resetCalendar $ unsafeToCalendarTime ct

-- | Parse a date string, assuming a default timezone if
--   the date string does not specify one.  The date formats
--   understood are those of 'showIsoDateTime' and 'dateTime'
parseDate :: Int -> String -> Either ParseError MCalendarTime
parseDate tz d =
              if length d >= 14 && B.all isDigit bd
              then Right $ toMCalendarTime $
                   CalendarTime (readI $ B.take 4 bd)
                                (toEnum $ (+ (-1)) $ readI $ B.take 2 $ B.drop 4 bd)
                                (readI $ B.take 2 $ B.drop 6 bd) -- Day
                                (readI $ B.take 2 $ B.drop 8 bd) -- Hour
                                (readI $ B.take 2 $ B.drop 10 bd) -- Minute
                                (readI $ B.take 2 $ B.drop 12 bd) -- Second
                                0 Sunday 0 -- Picosecond, weekday and day of year unknown
                                "GMT" 0 False
              else let dt = do { x <- dateTime tz; eof; return x }
                   in parse dt "" d
  where bd = B.pack (take 14 d)
        readI s = fst $ fromMaybe (error "parseDate: invalid date") (B.readInt s)

-- | Display a 'CalendarTime' in the ISO 8601 format without any
--   separators, e.g. 20080825142503
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

-- | The current time in the format returned by 'showIsoDateTime'
getIsoDateTime          :: IO String
getIsoDateTime = (showIsoDateTime . toUTCTime) `liftM` getClockTime

----- Parser Combinators ---------------------------------------------

-- | Case-insensitive variant of Parsec's 'char' function.
caseChar        :: Char -> GenParser Char a Char
caseChar c       = satisfy (\x -> toUpper x == toUpper c)

-- | Case-insensitive variant of Parsec's 'string' function.
caseString      :: String -> GenParser Char a ()
caseString cs    = mapM_ caseChar cs <?> cs

-- | Match a parser at least @n@ times.
manyN           :: Int -> GenParser a b c -> GenParser a b [c]
manyN n p
    | n <= 0     = return []
    | otherwise  = liftM2 (++) (count n p) (many p)

-- | Match a parser at least @n@ times, but no more than @m@ times.
manyNtoM        :: Int -> Int -> GenParser a b c -> GenParser a b [c]
manyNtoM n m p
    | n < 0      = return []
    | n > m      = return []
    | n == m     = count n p
    | n == 0     = foldr (<|>) (return []) (map (\x -> try $ count x p) (reverse [1..m]))
    | otherwise  = liftM2 (++) (count n p) (manyNtoM 0 (m-n) p)


----- Date/Time Parser -----------------------------------------------

-- | Try each of these date parsers in the following order
--
--    (1) 'cvsDateTime'
--
--    (2) 'iso8601DateTime'
--
--    (3) 'oldDateTime
dateTime :: Int -> CharParser a MCalendarTime
dateTime tz =
            choice [try $ toMCalendarTime `fmap` cvsDateTime tz,
                    try $ iso8601DateTime tz,
                    toMCalendarTime `fmap` oldDateTime]

-- | CVS-style date/times, e.g.
--   2007/08/25 14:25:39 GMT
--   Note that time-zones are optional here.
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

-- | \"Old\"-style dates, e.g.
--   Tue Jan 3 14:08:07 EST 1999
-- darcs-doc: Question (what does the "old" stand for really?)
oldDateTime   :: CharParser a CalendarTime
oldDateTime      = do wd <- dayName
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

-- | ISO 8601 dates and times.  Please note the following flaws:
--
--   I am reluctant to implement:
--
--      * years > 9999
--
--      * truncated representations with implied century (89 for 1989)
--
--   I have not implemented:
--
--      * repeated durations (not relevant)
--
--      * lowest order component fractions in intervals
--
--      * negative dates (BC)
--
--   I have not verified or have left too relaxed:
--
--      * the difference between 24h and 0h
--
--      * allows stuff like 2005-1212; either you use the hyphen all the way
--        (2005-12-12) or you don't use it at all (20051212), but you don't use
--        it halfway, likewise with time
--
--      * No bounds checking whatsoever on intervals!
--        (next action: read iso doc to see if bounds-checking required?) -}
iso8601DateTime   :: Int -> CharParser a MCalendarTime
iso8601DateTime localTz = try $
  do d <- iso8601Date
     t <- option id $ try $ do optional $ oneOf " T"
                               iso8601Time
     return $ t $ d { mctTZ = Just localTz }

-- | Three types of ISO 8601 date:
--
--     * calendar date, e.g., 1997-07-17, 1997-07, 199707, 1997
--
--     * week+day in year, e.g.,  1997-W32-4
--
--     * day in year, e.g, 1997-273
iso8601Date :: CharParser a MCalendarTime
iso8601Date =
  do d <- calendar_date <|> week_date <|> ordinal_date
     return $ foldr ($) nullMCalendar d
  where
    calendar_date = -- yyyy-mm-dd
      try $ do d <- optchain year_ [ (dash, month_), (dash, day_) ]
               -- allow other variants to be parsed correctly
               notFollowedBy (digit <|> char 'W')
               return d
    week_date = --yyyy-Www-d
      try $ do yfn <- year_
               optional dash
               _ <- char 'W'
               -- offset human 'week 1' -> computer 'week 0'
               w'  <- (\x -> x-1) `liftM` twoDigits
               mwd  <- option Nothing $ do { optional dash; Just `fmap` nDigits 1 }
               let y = resetCalendar . unsafeToCalendarTime . yfn $ nullMCalendar { mctDay = Just 1 }
                   firstDay = ctWDay y
               -- things that make this complicated
               -- 1. iso8601 weeks start from Monday; Haskell weeks start from Sunday
               -- 2. the first week is the one that contains at least Thursday
               --    if the year starts after Thursday, then some days of the year
               --    will have already passed before the first week
               let afterThursday = firstDay == Sunday || firstDay > Thursday
                   w  = if afterThursday then w'+1 else w'
                   yday = (7 * w) + fromMaybe 1 mwd
                   diff c = c { mctWeek = True
                              , mctWDay = toEnum `fmap` mwd
                              , mctDay  = Just yday }
               return [(diff.yfn)]
    ordinal_date = -- yyyy-ddd
      try $ optchain year_ [ (dash, yearDay_) ]
    --
    year_  = try $ do y <- fourDigits <?> "year (0000-9999)"
                      return $ \c -> c { mctYear = Just y }
    month_ = try $ do m <- twoDigits <?> "month (1 to 12)"
                      return $ \c -> c { mctMonth = Just $ intToMonth m }
    day_   = try $ do d <- twoDigits <?> "day in month (1 to 31)"
                      return $ \c -> c { mctDay = Just d }
    yearDay_ = try $ do d <- nDigits 3 <?> "day in year (001 to 366)"
                        return $ \c -> c { mctDay = Just d
                                         , mctYDay = Just (d - 1) }
    dash = char '-'

-- | Note that this returns a function which sets the time on
--   another calendar (see 'iso8601DateTime' for a list of
--   flaws
iso8601Time :: CharParser a (MCalendarTime -> MCalendarTime)
iso8601Time = try $
  do ts <- optchain hour_ [ (colon     , min_)
                          , (colon     , sec_)
                          , (oneOf ",.", pico_) ]
     z  <- option id $ choice [ zulu , offset ]
     return $ foldr (.) id (z:ts)
  where
    hour_ = do h <- twoDigits
               return $ \c -> c { mctHour = Just h }
    min_  = do m <- twoDigits
               return $ \c -> c { mctMin = Just m }
    sec_  = do s <- twoDigits
               return $ \c -> c { mctSec = Just s }
    pico_ = do digs <- many digit
               let picoExp = 12
                   digsExp = length digs
               let frac | null digs = 0
                        | digsExp > picoExp = read $ take picoExp digs
                        | otherwise = 10 ^ (picoExp - digsExp) * (read digs)
               return $ \c -> c { mctPicosec = Just $ frac }
    zulu   = do { _ <- char 'Z'; return (\c -> c { mctTZ = Just 0 }) }
    offset = do sign <- choice [ do { char '+' >> return   1  }
                               , do { char '-' >> return (-1) } ]
                h <- twoDigits
                m <- option 0 $ do { optional colon; twoDigits }
                return $ \c -> c { mctTZ = Just $ sign * 60 * ((h*60)+m) }
    colon = char ':'

-- | Intervals in ISO 8601, e.g.,
--
--    * 2008-09/2012-08-17T16:30
--
--    * 2008-09/P2Y11MT16H30M
--
--    * P2Y11MT16H30M/2012-08-17T16:30
--
--   See 'iso8601Duration'
iso8601Interval :: Int -> CharParser a (Either TimeDiff (MCalendarTime, MCalendarTime))
iso8601Interval localTz = leftDur <|> rightDur where
  leftDur  =
    do dur <- iso8601Duration
       end <- option Nothing $ do { _ <- char '/'; Just `liftM` isoDt }
       return $ case end of
                Nothing -> Left dur
                Just e  -> Right (dur `subtractFromMCal` e, e)
  rightDur =
    do start <- isoDt
       _ <- char '/'
       durOrEnd <- Left `liftM` iso8601Duration <|> Right `liftM` isoDt
       return $ case durOrEnd of
                Left dur  -> Right (start, dur `addToMCal` start)
                Right end -> Right (start, end)
  isoDt   = iso8601DateTime localTz

-- | Durations in ISO 8601, e.g.,
--
--    * P4Y (four years)
--
--    * P5M (five months)
--
--    * P4Y5M (four years and five months)
--
--    * P4YT3H6S (four years, three hours and six seconds)
iso8601Duration :: CharParser a TimeDiff
iso8601Duration =
  do _ <- char 'P'
     y   <- block 0 'Y'
     mon <- block 0 'M'
     d   <- block 0 'D'
     (h,m,s) <- option (0,0,0) $
       do _ <- char 'T'
          h' <- block (-1) 'H'
          m' <- block (-1) 'M'
          s' <- block (-1) 'S'
          let unset = (== (-1))
          if all unset [h',m',s']
             then fail "T should be omitted if time is unspecified"
             else let clear x = if (unset x) then 0 else x
                  in return (clear h', clear m', clear s')
     --
     return $ TimeDiff y mon d h m s 0
  where block d c = option d $ try $
          do n <- many1 digit
             _ <- char c
             return $ read n

-- | 'optchain' @p xs@ parses a string with the obligatory
--   parser @p@.  If this suceeds, it continues on to the
--   rest of the input using the next parsers down the
--   chain.  Each part of the chain consists of a parser
--   for a separator and for the content itself.  The
--   separator is optional.
--
--   A good use of this function is to help in parsing ISO
--   ISO 8601 dates and times.  For example, the parser
--   @optchain year [(dash, month), (dash, day)]@ accepts
--   dates like 2007 (only the year is used), 2007-07 (only
--   the year and month), 200707 (only the year and month
--   with no separator), 2007-07-19 (year, month and day).
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

-- | One or more space.
--   WARNING! This only matches on the space character, not on
--   whitespace in general
mySpaces :: CharParser a String
mySpaces = manyN 1 $ char ' '

-- | English three-letter day abbreviations (e.g. Mon, Tue, Wed)
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

-- | Four-digit year
year            :: CharParser a Int
year             = fourDigits

-- | One or two digit month (e.g. 3 for March, 11 for November)
monthNum       :: CharParser a Month
monthNum =  do mn <- manyNtoM 1 2 digit
               return $ intToMonth $ (read mn :: Int)

-- | January is 1, February is 2, etc
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

-- | English three-letter month abbreviations (e.g. Jan, Feb, Mar)
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

-- | day in one or two digit notation
day             :: CharParser a Int
day              = do d <- manyNtoM 1 2 digit
                      return (read d :: Int)

-- | hour in two-digit notation
hour            :: CharParser a Int
hour             = twoDigits

-- | minute in two-digit notation
minute          :: CharParser a Int
minute           = twoDigits

-- | second in two-digit notation
second          :: CharParser a Int
second           = twoDigits

-- | limited timezone support
--
--   * +HHMM or -HHMM
--
--   * Universal timezones: UTC, UT
--
--   * Zones from GNU coreutils/lib/getdate.y, less half-hour ones --
--     sorry Newfies.
--
--   * any sequence of alphabetic characters (WARNING! treated as 0!)
zone            :: CharParser a Int
zone             = choice
                       [ do { _ <- char '+'; h <- hour; m <- minute; return (((h*60)+m)*60) }
                       , do { _ <- char '-'; h <- hour; m <- minute; return (-((h*60)+m)*60) }
                       , mkZone "UTC"  0
                       , mkZone "UT"  0
                       , mkZone "GMT" 0
                       , mkZone "WET" 0
                       , mkZone "WEST" 1
                       , mkZone "BST" 1
                       , mkZone "ART" (-3)
                       , mkZone "BRT" (-3)
                       , mkZone "BRST" (-2)
                       , mkZone "AST" (-4)
                       , mkZone "ADT" (-3)
                       , mkZone "CLT" (-4)
                       , mkZone "CLST" (-3)
                       , mkZone "EST" (-5)
                       , mkZone "EDT" (-4)
                       , mkZone "CST" (-6)
                       , mkZone "CDT" (-5)
                       , mkZone "MST" (-7)
                       , mkZone "MDT" (-6)
                       , mkZone "PST" (-8)
                       , mkZone "PDT" (-7)
                       , mkZone "AKST" (-9)
                       , mkZone "AKDT" (-8)
                       , mkZone "HST" (-10)
                       , mkZone "HAST" (-10)
                       , mkZone "HADT" (-9)
                       , mkZone "SST" (-12)
                       , mkZone "WAT" 1
                       , mkZone "CET" 1
                       , mkZone "CEST" 2
                       , mkZone "MET" 1
                       , mkZone "MEZ" 1
                       , mkZone "MEST" 2
                       , mkZone "MESZ" 2
                       , mkZone "EET" 2
                       , mkZone "EEST" 3
                       , mkZone "CAT" 2
                       , mkZone "SAST" 2
                       , mkZone "EAT" 3
                       , mkZone "MSK" 3
                       , mkZone "MSD" 4
                       , mkZone "SGT" 8
                       , mkZone "KST" 9
                       , mkZone "JST" 9
                       , mkZone "GST" 10
                       , mkZone "NZST" 12
                       , mkZone "NZDT" 13
                         -- if we don't understand it, just give a GMT answer...
                       , do { _ <- manyTill (oneOf $ ['a'..'z']++['A'..'Z']++[' '])
                                       (lookAhead space_digit);
                              return 0 }
                       ]
     where mkZone n o  = try $ do { caseString n; return (o*60*60) }
           space_digit = try $ do { _ <- char ' '; oneOf ['0'..'9'] }

----- English dates and intervals -----------------------------------------------

-- | In English, either a date followed by a time, or vice-versa, e.g,
--
--    * yesterday at noon
--
--    * yesterday tea time
--
--    * 12:00 yesterday
--
--   See 'englishDate' and 'englishTime'
--   Uses its first argument as "now", i.e. the time relative to which
--   "yesterday", "today" etc are to be interpreted
englishDateTime :: CalendarTime -> CharParser a CalendarTime
englishDateTime now =
  try $ dateMaybeAtTime <|> timeThenDate
  where
   -- yesterday (at) noon
   dateMaybeAtTime = try $
     do ed <- englishDate now
        t  <- option Nothing $ try $
                do { _ <- space; optional $ caseString "at "; Just `liftM` englishTime }
        return $ fromMaybe id t $ ed
   -- tea time 2005-12-04
   timeThenDate = try $
     do t  <- englishTime
        optional $ char ','
        _ <- space
        ed <- englishDate now
        return $ t $ unsetTime $ ed

-- | Specific dates in English as specific points of time, e.g,
--
--    * today
--
--    * yesterday
--
--    * last week (i.e. the beginning of that interval)
--
--    * 4 months ago (via 'englishAgo')
--
--   The first argument is "now".
englishDate :: CalendarTime -> CharParser a CalendarTime
englishDate now = try $
      (caseString "today"     >> (return $ resetCalendar now))
  <|> (caseString "yesterday" >> (return $ oneDay `subtractFromCal` now) )
  <|> fst `fmap` englishLast now
  <|> englishAgo now
  where oneDay    = TimeDiff 0 0 1 0 0 0 0

-- | English expressions for points in the past, e.g.
--
--    * 4 months ago
--
--    * 1 day ago
--
--    * day before yesterday
--
--   See 'englishDuration'
englishAgo :: CalendarTime -> CharParser a CalendarTime
englishAgo now =
  try $ do p <- englishDuration
           _ <- try $ do space
           (m,ref) <- (try $ caseString "ago" >> return ((-1), now))
                   <|> do m <- beforeMod <|> afterMod
                          _ <- space
                          d <- englishDate now
                               <|> fst `fmap` englishLast now
                               <|> unsafeToCalendarTime `fmap` iso8601DateTime (ctTZ now)
                          return (m,d)
           return $ multiplyDiff m p `addToCal` ref
  where
    beforeMod = try $ caseString "before" >> return (-1)
    afterMod  = try $ caseString "after"  >> return 1

-- | English expressions for intervals of time,
--
--    * before tea time (i.e. from the beginning of time)
--
--    * after 14:00 last month (i.e. till now)
--
--    * between last year and last month
--
--    * in the last three months (i.e. from then till now)
--
--    * 4 months ago (i.e. till now; see 'englishAgo')
englishInterval :: CalendarTime -> CharParser a TimeInterval
englishInterval now = twixt <|> before <|> after <|> inTheLast <|> lastetc
  where
   englishDT = (unsafeToCalendarTime `fmap` iso8601DateTime (ctTZ now)
                <|> englishDateTime now)
   before = try $
     do caseString "before"
        _ <- space
        end <- englishDT
        return (Just theBeginning, Just end)
   after = try $
     do caseString "after"
        _ <- space
        start <- englishDT
        return (Just start, Nothing)
   twixt = try $
     do caseString "between"
        _ <- space
        start <- englishDT
        _ <- space
        caseString "and"
        _ <- space
        end <- englishDT
        return (Just start, Just end)
   inTheLast = try $
     do caseString "in the last"
        _ <- space
        dur <- englishDuration
        return (Just $ dur `subtractFromCal` now, Just now)
   lastetc =
     do l <- englishAgo now
        return (Just l, Just now)

-- | Durations in English that begin with the word \"last\",
--   E.g. \"last 4 months\" is treated as the duration between
--   4 months ago and now
englishLast :: CalendarTime -> CharParser a (CalendarTime, CalendarTime)
englishLast now =
    -- last year, last week, last 3 years, etc
    try $ do caseString "last"
             _ <- space
             d <- englishDuration
             return (d `subtractFromCal` now, now)

-- | Either an 'iso8601Time' or one of several common
--   English time expressions like 'noon' or 'tea time'
englishTime :: CharParser a (CalendarTime->CalendarTime)
englishTime = try $
  choice [ wrapM `fmap` iso8601Time
         , namedTime "noon"            12  0
         , namedTime "midnight"         0  0
         , namedTime "tea time"        16 30
         , namedTime "bed time"         2 30
         , namedTime "proper bed time" 21 30 ]
  where namedTime name h m = try $
          do caseString name
             return $ \c -> c { ctHour = h, ctMin = m }
        wrapM f = unsafeToCalendarTime . f . toMCalendarTime

-- | Some English durations, e.g.
--
--    * day
--
--    * 4 score
--
--    * 7 years
--
--    * 12 months
--
-- This is not particularly strict about what it accepts.
-- For example, "7 yeares", "4 scores" or "1 days" are
-- just fine.
englishDuration :: CharParser a TimeDiff
englishDuration = try $
  do n <- option 1 $ do { x <- many1 digit; _ <- space; (return $ read x) }
     b <- base
     optional (caseString "es" <|> caseString "s")
     let current = multiplyDiff n b
     next <- option noTimeDiff $ try $ do
              { optional space; _ <- char ',' ; optional space ; englishDuration }
     return $ addDiff current next
  where
  base = choice
         [ try $ caseString "score"      >> (return $ TimeDiff 20 0  0 0 0 0 0) -- why not?
         ,       caseString "year"       >> (return $ TimeDiff  1 0  0 0 0 0 0)
         , try $ caseString "month"      >> (return $ TimeDiff  0 1  0 0 0 0 0)
         ,       caseString "fortnight"  >> (return $ TimeDiff  0 0 14 0 0 0 0)
         ,       caseString "week"       >> (return $ TimeDiff  0 0  7 0 0 0 0)
         ,       caseString "day"        >> (return $ TimeDiff  0 0  1 0 0 0 0)
         ,       caseString "hour"       >> (return $ TimeDiff  0 0  0 1 0 0 0)
         ,       caseString "minute"     >> (return $ TimeDiff  0 0  0 0 1 0 0)
         ,       caseString "second"     >> (return $ TimeDiff  0 0  0 0 0 1 0) ]

----- Calendar and TimeDiff manipulation ---------------------------------------------

-- | The very beginning of time, i.e. 1970-01-01
theBeginning :: CalendarTime
theBeginning = unsafePerformIO $ toCalendarTime $ TOD 0 0

-- | An 'MCalenderTime' is an underspecified 'CalendarTime'
--   It is used for parsing dates.  For example, if you want to parse
--   the date '4 January', it may be useful to underspecify the year
--   by setting it to 'Nothing'.  This uses almost the same fields as
--   'System.Time.CalendarTime', a notable exception being that we
--   introduce 'mctWeek' to indicate if a weekday was specified or not
data MCalendarTime = MCalendarTime
 { mctYear  :: Maybe Int
 , mctMonth :: Maybe Month
 , mctDay   :: Maybe Int
 , mctHour  :: Maybe Int
 , mctMin   :: Maybe Int
 , mctSec   :: Maybe Int
 , mctPicosec :: Maybe Integer
 , mctWDay     :: Maybe Day
 , mctYDay     :: Maybe Int
 , mctTZName   :: Maybe String
 , mctTZ       :: Maybe Int
 , mctIsDST    :: Maybe Bool
 , mctWeek     :: Bool -- is set or not
} deriving Show

-- | Trivially convert a 'CalendarTime' to a fully specified
--   'MCalendarTime' (note that this sets the 'mctWeek' flag to
--   @False@
toMCalendarTime :: CalendarTime -> MCalendarTime
toMCalendarTime (CalendarTime a b c d e f g h i j k l) =
  MCalendarTime (Just a) (Just b) (Just c) (Just d) (Just e) (Just f)
                (Just g) (Just h) (Just i) (Just j) (Just k) (Just l)
                False

-- | Returns the first 'CalendarTime' that falls within a 'MCalendarTime'
--   This is only unsafe in the sense that it plugs in default values
--   for fields that have not been set, e.g. @January@ for the month
--   or @0@ for the seconds field.
--   Maybe we should rename it something happier.
--   See also 'resetCalendar'
unsafeToCalendarTime :: MCalendarTime -> CalendarTime
unsafeToCalendarTime m =
 CalendarTime
  { ctYear = fromMaybe 0 $ mctYear m
  , ctMonth = fromMaybe January $ mctMonth m
  , ctDay = fromMaybe 1 $ mctDay m
  , ctHour = fromMaybe 0 $ mctHour m
  , ctMin = fromMaybe 0 $ mctMin m
  , ctSec = fromMaybe 0 $ mctSec m
  , ctPicosec = fromMaybe 0 $ mctPicosec m
  , ctWDay = fromMaybe Sunday $ mctWDay m
  , ctYDay = fromMaybe 0 $ mctYDay m
  , ctTZName = fromMaybe "" $ mctTZName m
  , ctTZ = fromMaybe 0 $ mctTZ m
  , ctIsDST = fromMaybe False $ mctIsDST m
 }

addToCal :: TimeDiff -> CalendarTime -> CalendarTime
addToCal td = toUTCTime . addToClockTime td . toClockTime

subtractFromCal :: TimeDiff -> CalendarTime -> CalendarTime
subtractFromCal = addToCal . multiplyDiff (-1)

addToMCal :: TimeDiff -> MCalendarTime -> MCalendarTime
addToMCal td mc =
 copyCalendar (addToCal td $ unsafeToCalendarTime mc) mc

subtractFromMCal :: TimeDiff -> MCalendarTime -> MCalendarTime
subtractFromMCal = addToMCal . multiplyDiff (-1)

-- surely there is a more concise way to express these
addDiff :: TimeDiff -> TimeDiff -> TimeDiff
addDiff (TimeDiff a1 a2 a3 a4 a5 a6 a7) (TimeDiff b1 b2 b3 b4 b5 b6 b7) =
  TimeDiff (a1+b1) (a2+b2) (a3+b3) (a4+b4) (a5+b5) (a6+b6) (a7 + b7)

-- | 'multiplyDiff' @i d@ multiplies every field in @d@ with @i@
--
-- FIXME; this seems like a terrible idea! it seems like
-- we should get rid of it if at all possible, maybe adding an
-- invertDiff function
multiplyDiff :: Int -> TimeDiff -> TimeDiff
multiplyDiff m (TimeDiff a1 a2 a3 a4 a5 a6 a7) =
  TimeDiff (a1*m) (a2*m) (a3*m) (a4*m) (a5*m) (a6*m) (a7 * (toInteger m))

nullMCalendar :: MCalendarTime
nullMCalendar = MCalendarTime Nothing Nothing Nothing Nothing Nothing Nothing
                              Nothing Nothing Nothing Nothing Nothing Nothing
                              False

-- | Set a calendar to UTC time any eliminate any inconsistencies within
--   (for example, where the weekday is given as @Thursday@, but this does not
--   match what the numerical date would lead one to expect)
resetCalendar :: CalendarTime -> CalendarTime
resetCalendar = toUTCTime . toClockTime

-- | 'copyCalendar' @c mc@ replaces any field which is
--   specified in @mc@ with the equivalent field in @c@
--   @copyCalendar c nullMCalendar == nullMCalendar@
copyCalendar :: CalendarTime -> MCalendarTime -> MCalendarTime
copyCalendar c mc = mc
  { mctYear  = mctYear mc  >> Just (ctYear c)
  , mctMonth = mctMonth mc >> Just (ctMonth c)
  , mctDay   = mctDay mc   >> Just (ctDay c)
  , mctHour  = mctHour mc  >> Just (ctHour c)
  , mctMin   = mctMin mc   >> Just (ctMin c)
  , mctSec   = mctSec mc   >> Just (ctSec c)
  , mctPicosec = mctPicosec mc >> Just (ctPicosec c)
  , mctWDay = mctWDay mc   >> Just (ctWDay c)
  , mctYDay = mctYDay mc   >> Just (ctYDay c)
  , mctTZName = mctTZName mc >> Just (ctTZName c)
  , mctTZ     = mctTZ mc    >> Just (ctTZ c)
  , mctIsDST  = mctIsDST mc >> Just (ctIsDST c)
  }

-- | Zero the time fields of a 'CalendarTime'
unsetTime :: CalendarTime -> CalendarTime
unsetTime mc = mc
  { ctHour  = 0
  , ctMin   = 0
  , ctSec   = 0
  , ctPicosec = 0
  }
