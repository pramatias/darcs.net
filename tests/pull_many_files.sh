#!/usr/bin/env bash
. lib

rm -rf temp1 temp2

if grep old-fashioned .darcs/defaults; then
format=old-fashioned-inventory
elif grep darcs-2 .darcs/defaults; then
format=darcs-2
else format=hashed; fi

mkdir temp2
cd temp2
gunzip -c $TESTDATA/many-files--${format}.tgz | tar xf -
cd ..

mkdir temp1
cd temp1
darcs init
darcs pull -a ../temp2/many-files--${format} > log
grep -i 'finished pulling' log
cd ..
rm -rf temp1

# put things back how we found them.

rm -rf temp1 temp2
