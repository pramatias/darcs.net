#!/usr/bin/env bash

. lib

# This test script is in the public domain.


rm -rf temp1 temp2 temp3

mkdir temp1
cd temp1
darcs initialize
echo A > A
darcs add A
darcs record -a -m A
echo B > B
darcs add B
darcs record -a -m B

cd ..
darcs get temp1 temp2

cd temp2
darcs obliterate --last 1 -a
echo C > C
darcs add C
darcs record -a -m C
cd ..

mkdir temp3
cd temp3
darcs init
darcs pull -a -v ../temp1 ../temp2

darcs changes > out
cat out
grep A out
grep B out
grep C out
cd ..

rm -rf temp1 temp2 temp3
