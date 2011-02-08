-- Copyright (C) 2004 David Roundy
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


{-# LANGUAGE ExistentialQuantification #-}

module DateMatcher ( parseDateMatcher
                   -- for debugging only
                   , DateMatcher(..), getMatchers ) where

import Prelude hiding ( catch )
import Control.Exception.Extensible ( catch, throw )
import System.IO.Error ( isUserError, ioeGetErrorString )
import Data.Maybe ( isJust )
import System.Time
import IsoDate ( parseDate, englishDateTime, englishInterval, englishLast, iso8601Interval,
                 resetCalendar, subtractFromMCal, getLocalTz,
                 MCalendarTime(..), toMCalendarTime, unsafeToCalendarTime,
                 unsetTime,
               )
import Text.ParserCombinators.Parsec ( eof, parse, ParseError )

-- | 'withinDay' @x y@ is true if @x <= y < (x + one_day)@
-- Note that this converts the two dates to @ClockTime@ to avoid
-- any timezone-related errors
withinDay :: CalendarTime -> CalendarTime -> Bool
withinDay a b = within (Just $ toClockTime a)
                       (Just (addToClockTime day $ toClockTime a))
                       (toClockTime b)
 where day = TimeDiff 0 0 1 0 0 0 0

-- | 'dateRange' @x1 x2 y@ is true if @x1 <= y < x2@
--   Since @x1@ and @x2@ can be underspecified, we simply assume the
--   first date that they could stand for.
dateRange :: Maybe MCalendarTime -> Maybe MCalendarTime -> CalendarTime -> Bool
dateRange a b c = cDateRange (fmap unsafeToCalendarTime a)
                             (fmap unsafeToCalendarTime b) c

-- | 'cDateRange' @x1 x2 y@ is true if @x1 <= y < x2@
cDateRange :: Maybe CalendarTime -> Maybe CalendarTime -> CalendarTime -> Bool
cDateRange a b c = within (fmap toClockTime a)
                          (fmap toClockTime b) (toClockTime c)

-- | 'within' @x1 x2 y@ is true if @x1 <= y < x2@
within :: Maybe ClockTime -> Maybe ClockTime -> ClockTime -> Bool
within (Just start) (Just end) time = start <= time && time < end
within Nothing (Just end) time = time < end
within (Just start) Nothing time = start <= time
within _ _ _ = undefined

-- | 'samePartialDate' @range exact@ is true if @exact@ falls
--   within the a range of dates represented by @range@.
--   The purpose of this function is to support matching on partially
--   specified dates.  That is, if you only specify the date 2007,
--   this function should match any dates within that year.  On the
--   other hand, if you specify 2007-01, this function will match any
--   dates within that month.  This function only matches up to the
--   second.
samePartialDate :: MCalendarTime -> CalendarTime -> Bool
samePartialDate a b_ =
 within (Just clockA)
        (Just $ addToClockTime interval clockA)
        (toClockTime calB)
 where interval
         | isJust (mctSec a)   = second
         | isJust (mctMin a)   = minute
         | isJust (mctHour a)  = hour
         | isJust (mctYDay a)  = day
         | mctWeek a = maybe week (const day) (mctWDay a)
         | isJust (mctDay a)   = day
         | isJust (mctMonth a) = month
         | otherwise           = year
       year  = TimeDiff 1 0 0 0 0 0 0
       month = TimeDiff 0 1 0 0 0 0 0
       week  = TimeDiff 0 0 7 0 0 0 0
       day   = TimeDiff 0 0 1 0 0 0 0
       hour   = TimeDiff 0 0 0 1 0 0 0
       minute = TimeDiff 0 0 0 0 1 0 0
       second = TimeDiff 0 0 0 0 0 1 0
       --
       clockA = toClockTime $ unsafeToCalendarTime a
       calB   = resetCalendar b_

-- | A 'DateMatcher' combines a potential parse for a date string
--   with a "matcher" function that operates on a given date.
--   We use an existential type on the matcher to allow
--   the date string to either be interpreted as a point in time
--   or as an interval.
data DateMatcher = forall d . (Show d) =>
       DM String --  name
          (Either ParseError d) --  parser
          (d -> CalendarTime -> Bool) --  matcher

-- | 'parseDateMatcher' @s@ return the first  matcher in
--    'getMatchers' that can parse 's'
parseDateMatcher :: String -> IO (CalendarTime -> Bool)
parseDateMatcher d =
 do matcher <- tryMatchers `fmap` getMatchers d
    -- Hack: test the matcher against the current date and discard the results.
    -- We just want to make sure it won't throw any exceptions when we use it for real.
    matcher `fmap` now >>= (`seq` return matcher)
 `catchUserError`
    -- If the user enters a date > maxint seconds ago, the toClockTime
    -- function cannot work.
    \e -> if e == "Time.toClockTime: invalid input"
          then error "Can't handle dates that far back!"
          else error e
 where
   catchUserError comp handler
    = catch comp (\e -> if isUserError e then handler (ioeGetErrorString e) else throw e)

-- | 'getMatchers' @d@ returns the list of matchers that will be
--   applied on @d@.  If you wish to extend the date parsing code,
--   this will likely be the function that you modify to do so.
getMatchers :: String -> IO [DateMatcher]
getMatchers d =
 do rightNow <- now
    let midnightToday = unsetTime rightNow
        mRightNow = toMCalendarTime rightNow
        matchIsoInterval (Left dur) = dateRange (Just $ dur `subtractFromMCal` mRightNow) (Just mRightNow)
        matchIsoInterval (Right (a,b)) = dateRange (Just a) (Just b)
    tzNow <- getLocalTz
    return -- note that the order of these is quite important as some matchers
           -- can match the same date.
          [ DM "from English date"
                (parseDateWith $ englishLast midnightToday)
                (\(a,_) -> cDateRange (Just a) Nothing)
          , DM "specific English date"
                (parseDateWith $ englishDateTime midnightToday)
                withinDay
          , DM "English interval"
                (parseDateWith $ englishInterval rightNow)
                (uncurry cDateRange)
          , DM "ISO 8601 interval"
                (parseDateWith $ iso8601Interval tzNow)
                matchIsoInterval
          , DM "CVS, ISO 8601, or old style date"
                (parseDate tzNow d)
                samePartialDate ]
 where
   tillEof p = do { x <- p; eof; return x }
   parseDateWith p = parse (tillEof p) "" d

-- | 'tryMatchers' @ms@ returns the first successful match in @ms@
--   It is an error if there are no matches
tryMatchers :: [DateMatcher] -> (CalendarTime -> Bool)
tryMatchers (DM _ parsed matcher : ms) =
  case parsed of
  Left _   -> tryMatchers ms
  Right  d -> matcher d
tryMatchers [] = error "Can't support fancy dates."

-- darcs-doc: self-explanatory
now :: IO CalendarTime
now = getClockTime >>= toCalendarTime

