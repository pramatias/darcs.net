#!/usr/bin/env bash

set -ev

rm -rf temp1

mkdir temp1
cd temp1
darcs init
echo -e 'foo\nbar\nbaz' > f
darcs rec -Ax -alm p1
echo -e 'foo\nbar\nwibble' > f
darcs rec -Ax -alm p2
sleep 1 # ensure the timestamps would differ after this change alone
echo -e 'baz\nbar\nwibble' > f

# check that wh (without --ignore-times) sees the change now
darcs wh > whatsnew
grep 'foo' whatsnew

# the problematic unpull
darcs unpull --last 1 -a --ignore-times

# whatsnew will now think there are no changes without --ignore-times
darcs wh > whatsnew
grep 'foo' whatsnew

cd ..
rm -rf temp1
