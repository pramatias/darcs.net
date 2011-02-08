#!/bin/env bash
#
# Test darcs conflict fight scenario.
#
# Set up two repos RA and RB. Create conflict in RB. 
# After resolving conflict in RB, pull new patch from RA.
# Repeat, rinse.
#
# Author: Pekka Pessi
#

set -ev

record="record --ignore-time --all --author X"

rm -rf RA RB
mkdir RA

cd RA
echo 0 > file
darcs init
darcs add file
darcs $record -m0 file
cd ..

darcs get RA RB

# Create conflict in RB
cd RB
echo let it b > file
darcs $record -m B
cd ..

for i in 1 2 3 4 5 # 6 7 8 9 10 11 12
do
  echo Create new patch A$i in RA
  cd RA
  echo a$i > file
  darcs $record -m A$i
  cd ..

  echo Pull patch A$i from RA and get a conflict
  cd RB
  time darcs pull ../RA --verbose --all --patch "^A$i\$"
  cd ..

  echo Resolve conflict and start fighting by recording B$i
  cd RB
  echo let it b > file
  darcs $record -m B$i
  cd ..
done

rm -rf RA RB
