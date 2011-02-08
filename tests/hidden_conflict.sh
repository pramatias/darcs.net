#!/bin/sh

set -ev

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init --darcs-2
echo first > a
darcs add a
darcs record -am 'first'
cd ..
darcs get temp1 temp2

cd temp1
echo second > a
darcs record -am 'first to second'
echo first > a
darcs record -am 'second back to first'
cd ..

cd temp2
echo third > a
darcs record -am 'first to third'
cd ..

cd temp1
darcs pull -a ../temp2 | grep conflict
grep third a
cd ..

rm -rf temp1 temp2
