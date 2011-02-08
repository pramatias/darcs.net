#!/usr/bin/env bash
. lib

# Some tests for 'darcs tag'

rm -rf temp1 log
mkdir temp1
cd temp1
darcs init
touch one
darcs add one
darcs record --name 'uno' --all
darcs tag  soup > log
not grep failed log
grep TAG log
darcs changes --last 1 > log
grep tagged log
cd ..
rm -rf temp1 log
