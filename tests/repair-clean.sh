#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1

darcs init
touch baz
darcs add baz
darcs record -m moo -a

cat _darcs/patches/pending

darcs changes -v

darcs check

# check that repair doesn't do anything to a clean repository
darcs repair > out
cat out
grep 'already consistent' out

cd ..
rm -rf temp1
