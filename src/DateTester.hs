-- Copyright (C) 2008 Eric Kow
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

-- To be loaded with GHCi

-- make ghci
-- Prelude>:l DateTester
-- *DateTester>testDate   "2008/05/22 10:34"
-- *DateTester>testDateAt "2006-03-22 09:36" "2008/05/22 10:34"

-- You could also just compile it if you want, but I don't see
-- the point.

module DateTester where

import DateMatcher
import IsoDate
import System.Time

-- | 'testDate' @d@ shows the possible interpretations
--   for the date string @d@ and how they match against
--   the current date
testDate :: String -> IO ()
testDate d =
 do now <- getClockTime >>= toCalendarTime
    testDateAtCal now d

-- | 'testDate' @iso d@ shows the possible interpretations
--   for the date string @d@ and how they match against
--   the date represented by the ISO 8601 string @iso@
testDateAt :: String -> String -> IO ()
testDateAt iso d = testDateAtCal (readUTCDate iso) d

-- | helper function for 'testDate' and 'testDateAt'
testDateAtCal :: CalendarTime -> String -> IO ()
testDateAtCal c d =
 do ms <- getMatchers d
    putStr . unlines . map (showMatcher c) $ ms

-- | 'showMatcher' @c dm@ tells us if @dm@ applies to
--   'CalendarTime' @c@; or if @dm@ just represents the
--   failure to parse a date, in which case @c@ is moot.
showMatcher :: CalendarTime -> DateMatcher -> String
showMatcher now (DM n p m) =
   "==== " ++ n ++ " ====\n" ++
   (case p of
     Left err -> shows err ""
     Right x  -> show x ++ "\n" ++ (show $ m x now))
