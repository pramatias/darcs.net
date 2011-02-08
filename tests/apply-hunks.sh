#!/usr/bin/env bash
set -ev

rm -rf temp0 temp1 temp2

# step 1
mkdir temp0
cd temp0
darcs init --darcs-2
echo m1 > foo
darcs add foo
darcs record -a -m m1 -A moi --ignore-times
cd ..

# step 2
darcs get temp0 temp1
cd temp1
echo a1 > foo
darcs record foo -a -m a1 -A moi --ignore-times
cd ..


# step 3
cd temp0
echo m2 > foo
darcs record -a -m m2 -A moi --ignore-times
cd ..


# step 4
cd temp1
darcs pull -a
echo m2-a1 > foo
darcs record -a -m 'Fix conflict m2-a1' -A moi --ignore-times
cd ..

#step 5
cd temp0
echo m3 > foo
darcs record -a -m m3 -A moi --ignore-times
cd ..

#step 6
darcs get temp0 temp2
cd temp2
echo b1 > foo
darcs record -a -m b1 -A moi --ignore-times
cd ..

#step 7
cd temp0
echo m4 > foo
darcs record -a -m m4 -A moi --ignore-times
cd ..

#step 8
cd temp1
darcs pull -a
echo m2-a1-m4 > foo
echo y | darcs mark-conflicts
cd ..

rm -rf temp0 temp1 temp2
