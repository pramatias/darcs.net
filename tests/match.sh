#!/usr/bin/env bash

# Some tests for the '--match' flag

. lib

# set up the repository
rm -rf temp1                    # another script may have left a mess.
mkdir temp1
cd temp1
darcs init
cd ..

# create three patches - the property we exploit to determine
# if a matcher does the right thing is that each patch has a
# different author
cd temp1
touch bar
darcs add bar
darcs record -a -m "first patch"  bar -A author1
echo foo > bar
darcs record -a -m "\"second\" \\ patch" bar -A author2
echo blop > bar
darcs record -a -m "second" bar -A author3
cd ..

# -------------------------------------------------------------------
# single matchers
# -------------------------------------------------------------------

cd temp1
# matching on author really matches on that, and not something else
darcs changes --match='author "first patch"' > log
not grep '.' log
# normal changes shows both authors and patch names
darcs changes > log
grep author1 log
grep author2 log
grep author3 log
grep 'first patch' log
grep '"second" \\ patch' log
grep -v patch log | grep second
# exact
darcs changes --match='exact second' > log
not grep author1 log
not grep author2 log
grep author3 log
# name
darcs changes --match='name second' > log
not grep author1 log
grep author2 log
grep author2 log
# author
darcs changes --match='author author1' > log
grep author1 log
not grep author2 log
not grep author3 log
#hash
darcs changes --xml-output --match='exact "\"second\" \ patch"' > log
hash=`grep hash log | sed -e "s/.*hash='//" -e "s/'.*//"`
echo $hash
darcs changes --match="hash $hash"
not grep author1 log
grep author2 log
not grep author3 log
cd ..

# -------------------------------------------------------------------
# matching on combinations
#
# uses the setup from the atomic patches
# -------------------------------------------------------------------

cd temp1
# or
darcs changes --match='author author1 || author author2' > log
grep author1 log
grep author2 log
not grep author3 log
# and
darcs changes --match='name second && author author2' > log
not grep author1 log
grep author2 log
not grep author3 log
# not
darcs changes --match='not name second' > log
grep author1 log
not grep author2 log
not grep author3 log
# grouping
darcs changes --match='(not name second) || (author author3)' > log
grep author1 log
not grep author2 log
grep author3 log
cd ..

rm -rf temp1
