#!/bin/sh

# Issue885: Regression: "darcs get --to-match" does not work anymore under 2.0


. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo first > a
darcs add a
darcs record -am 'first'
firsthash=`darcs changes --xml | grep 'hash=' | sed -e "s/.*hash='//" -e "s/'>//"`
echo second > b
darcs add b
darcs record -am 'second'

# Pulling that patch works ok
cd ..
rm -rf temp2
mkdir temp2
cd temp2
darcs init
echo darcs pull -v -a --match "hash $firsthash" ../temp1
darcs pull -v -a --match "hash $firsthash" ../temp1

# Getting up-to that patch does not
cd ..
rm -rf temp3
echo darcs get -v --to-match "hash $firsthash" temp1 temp3
darcs get -v --to-match "hash $firsthash" temp1 temp3

