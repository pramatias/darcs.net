#!/usr/bin/env bash
set -ev

# Some tests for 'darcs changes'

rm -rf temp1
mkdir temp1
cd temp1
darcs init

date >> date.t
darcs add date.t

darcs record -A 'Mark Stosberg <a@b.com>' -a -m foo date.t

####

darcs changes date.t > out # trivial case first
cat out
grep foo out

darcs changes --last=1 date.t > out
cat out
grep foo out

darcs changes --last 1 --summary date.t > out
cat out
grep foo out

darcs changes --last=1 --xml > out
cat out
grep '&lt;a@b.com&gt;' out # check that --xml encodes < and >

###

# Add 6 records and try again
for i in 0 1 2 3 4 5; do
    date >> date.t
    darcs record -a -m "foo record num $i" date.t
done

darcs changes date.t > out
cat out
grep foo out

darcs changes --last=1 date.t > out
cat out
grep foo out

darcs changes --last 1 --summary date.t > out
cat out
grep foo out

###

darcs changes --context --from-patch='num 1' --to-patch 'num 4' > out
cat out
grep 'num 4' out
grep 'num 3' out
grep 'num 2' out
grep 'num 1' out

###

date >> second_file.t
darcs add second_file.t

darcs record -a -m adding_second_file second_file.t

cd ..
rm -rf temp1
