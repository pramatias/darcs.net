#!/usr/bin/env bash
set -ev

rm -rf tempA tempB
mkdir tempA tempB
cd tempA
darcs init
touch foo
darcs add foo
darcs record -a -m addA -A x
cd ../tempB
darcs init
darcs pull -a ../tempA
darcs remove foo
# rm foo
darcs record -a -m rmB -A x
cd ../tempA
darcs remove foo
darcs record -a -m rmA -A x
cd ../tempB
darcs pull -a ../tempA
cd ..
rm -rf tempA tempB

