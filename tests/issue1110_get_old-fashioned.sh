#!/usr/bin/env bash
set -ev

rm -rf temp0 temp1
# sanity check
mkdir temp0
cd temp0
darcs init --old-fashioned
cd ..
darcs get --old-fashioned temp0 temp1
test -e temp1/_darcs/inventory
rm -rf temp0 temp1

# get from hashed
mkdir temp0
cd temp0
darcs init --hashed
cd ..
darcs get --old-fashioned temp0 temp1
test -e temp1/_darcs/inventory
rm -rf temp0 temp1

# get from darcs-2
mkdir temp0
cd temp0
darcs init --darcs-2
cd ..
darcs get --old-fashioned temp0 temp1 > log
grep 'Warning' log
rm -rf temp0 temp1
