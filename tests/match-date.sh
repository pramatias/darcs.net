#!/usr/bin/env bash

. lib

rm -rf temp1
# Some tests for the '--match' flag, specifically the date-matching

fmt_offset() {
    if date -d "now${1}days" >& /dev/null; then
        date -d "now${1}days" +"%Y%m%d"
    elif date -v ${1}d >& /dev/null; then
        date -v ${1}d +"%Y%m%d"
    else
        echo "Can't do date arithmetic on this system :(" >&2
        return 1
    fi
}

reset_repo () {
  cd ..
  rm -rf temp1
  mkdir temp1
  cd temp1
  darcs init
  touch bar
  darcs add bar
}

create_entry () {
  echo $1 >> bar
  echo "$1/tester/a///" | tr / \\012 | darcs record -m "" --pipe bar
}

create_entry_now () {
  echo today >> bar
  darcs record -m "" bar -a -A tester
}

# parse_date just checks for parsing, while match_date checks for an actual match.
# It's better if we can use "match_date", but we have to be able to construct such a date
# based on a date dynamically generated by this test script.

# alternately, it might be more useful to build a random date string generator
# using QuickCheck... for any n random CalendarTimes, have it generate some
# possible variants and roundtrip them to see if they match

parse_date () {
  darcs changes --match "date \"$1\"" > log
  not grep fancy log
}

match_date () {
  darcs changes --match "date \"$1\"" > log
  grep tester log
  not grep fancy log
}

nomatch_date () {
  darcs changes --match "date \"$1\"" > log
  not grep tester log
  not grep fancy  log
}

mkdir temp1
cd temp1
reset_repo

# this block of dates should all refer to the same thing
year=1973
mm=02
dd=04
hhmmss="15:08"
create_entry "$year-$mm-$dd $hhmmss"
echo "/tester/a///" | tr / \\012 | darcs record -m "" --pipe bar
match_date "$year-$mm-$dd"
match_date "$year$mm$dd"
match_date "$year-$mm"
match_date "$year$mm"
match_date "$year"

# week dates. note that 2007 was selected as it starts on Monday
reset_repo
create_entry "2007-01-04 15:00"
match_date '2007-W01-4'
nomatch_date '2007-W01-1'
match_date '2007W014'
match_date '2007-W01'
nomatch_date '2007-W02-1'
create_entry "2007-01-08 15:00"
match_date '2007-W02'
match_date '2007-W02-1'
create_entry "2007-05-20 15:00"
match_date '2007-W20'
nomatch_date '2007-W21'
nomatch_date '2007-W19'
# ordinal dates. eh... why not?
match_date '2007-004'; # fourth day of 2007
match_date '2007004'
nomatch_date '2007-005'

# midnight and zero
reset_repo
create_entry "1992-10-15 00:00"
match_date '1992-10-14 24:00'
match_date '1992-10-15 00:00'

# all the same date/time
reset_repo
create_entry "1992-02-12T22:32:11"
match_date '1992-02-12T22:32:11'
match_date '1992-02-12 22:32:11'
match_date '1992-02-12T223211.0000'

# english dates - the old hard coded from < darcs 1.0.6
reset_repo
year=`date +%Y`
mm=`date +%m`
dd=`date +%d`
hhmmss=`date +%k:%M:%S`
tz=`date +%z`
create_entry "$(($year-1))-$mm-$dd"
nomatch_date 'today'
nomatch_date 'yesterday'
nomatch_date 'day before yesterday'
nomatch_date 'last week'
nomatch_date 'last month'
# note: this test might fail if you run it just before midnight
reset_repo
create_entry_now
match_date 'today'
nomatch_date 'yesterday'
nomatch_date 'day before yesterday'
match_date 'last week'
match_date 'last month'

fmt_offset -1 || exit 200

reset_repo
create_entry "$(fmt_offset -350)"
# english dates - new possibilities
nomatch_date 'yesterday at 14:00:00'
match_date 'last 3 years'
match_date 'last year'
nomatch_date '2 days ago'
nomatch_date 'last month 13:00'
nomatch_date '3 days before last week'
reset_repo
create_entry_now
match_date 'day after yesterday'
match_date 'week after last week'
create_entry "1992-10-02 00:15"
match_date '15 minutes after 1992-10-02'
reset_repo
create_entry "1992-10-02 00:15+05"
# note that earlier dates will always match
match_date '15 minutes after 1992-10-02 00:00+05';   # same time
match_date '15 minutes after 1992-10-01 23:00+04';   # same time
nomatch_date '15 minutes after 1992-10-02 01:00+05'; # 1 hour later
nomatch_date '15 minutes after 1992-10-02 00:00+04'; # 1 hour later
nomatch_date '1 hour, 15 minutes after 1992-10-02 00:00+05'; # 1 hour later
match_date '1 hour, 15 minutes after 1992-10-02 00:00+06'; # same time
match_date '1 hour, 15 minutes after 1992-10-01 23:00+05'; # same time

reset_repo
create_entry_now
create_entry 1992-10-02 00:15
# english intervals
nomatch_date 'between last fortnight and day before yesterday'
match_date 'between last fortnight and today'
match_date 'in the last 45 seconds'
match_date 'after 1992'

# iso 8601 intervals
parse_date '1992-10-02 00:00Z/1992-10-02 00:16Z'
match_date '1992-10-02 00:00/1992-10-02 00:16'
match_date 'between 1992-10-02 00:00 and 1992-10-12 00:16'
parse_date 'P3YT3M/1992'
parse_date '1992/P3Y3M4DT5H3M2S'
parse_date '1992/P3Y3M'

# stuff from the manual
reset_repo
create_entry_now
nomatch_date 'between 2004-03-12 and last week'
match_date 'last week'
parse_date 'yesterday'
parse_date 'today 14:00'
nomatch_date '3 days before last year at 17:00'
# We can't in general parse the raw date output by darcs.  If we change the
# format to not include timezone information, this would be possible.  But
# maybe that's not desireable.  For now, we just won't test the raw date.
#match_date "$raw_date"
parse_date 'after 2005'
parse_date 'in the last 3 weeks'
parse_date 'P3M/2006-03-17'
parse_date '2004-01-02/2006-03-17'
parse_date 'P2M6D'

# cvs dates
parse_date '2006/01/19 21:14:20 UTC'
# We can't handle all timezones in the old style dates
# so this test will not work everywhere
# match_date "$year/$mm/$dd $hhmmss $tz"

reset_repo
create_entry '2038-01-01'
match_date 'after 2037'

rm -rf temp1 temp2
