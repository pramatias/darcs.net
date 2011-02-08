#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2 temp3
mkdir -p temp1 temp2 temp3
cd temp1
darcs init
cd ../temp2
darcs init
cd ../temp3
darcs init

touch foo
darcs add foo
darcs record -a -A author -m add
darcs push -a ../temp2
darcs push -a ../temp1

echo A > foo
darcs record -a -A author -m AA
cd ../temp2
echo B > foo
darcs record -a -A author -m BB
cd ../temp1
echo C > foo
darcs record -a -A author -m CC

darcs pull -a ../temp2
darcs pull -a ../temp3

cd ../temp2
darcs pull -a ../temp3
darcs pull -a ../temp1
rm -rf temp1 temp2 temp3
