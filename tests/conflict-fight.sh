#!/usr/bin/env bash

set -ev

rm -rf temp0 temp1 temp2

# step 1
mkdir temp0
cd temp0
darcs init --darcs-2
echo temp0 > _darcs/prefs/author
echo m1 > foo
darcs add foo
darcs record -a -m m1 --ignore-times
cd ..

# step 2
darcs get temp0 temp1
cd temp1
echo temp1 > _darcs/prefs/author
echo a1 > foo
darcs record foo -a -m a1 --ignore-times
cd ..


# step 3
cd temp0
echo m2 > foo
darcs record -a -m m2 --ignore-times
cd ..


# step 4
cd temp1
darcs pull -a
echo m2-a1 > foo
darcs record -a -m 'Fix conflict m2-a1' --ignore-times
echo a2 > foo
darcs record -a -m a2 --ignore-times
cd ..

#step 5
cd temp0
echo m3 > foo
darcs record -a -m m3 --ignore-times
cd ..

#step 6
darcs get temp0 temp2
cd temp2
echo temp2 > _darcs/prefs/author
echo b1 > foo
darcs record -a -m b1 --ignore-times

cd ..

#step 7
cd temp0
echo m4 > foo
darcs record -a -m m4 --ignore-times
cd ..

#step 8
cd temp1
darcs pull -a
echo m2-a1-m4 > foo
darcs record -a -m 'Fix three-way m2/m2-a1/m4' --ignore-times
echo a3 > foo
darcs record -a -m a3 --ignore-times
cd ..

#step 9
cd temp1
darcs pull -av ../temp2
cd ..

rm -rf temp0 temp1 temp2

