#!/usr/bin/env bash

set -ev

# Check that the right patches get pushed using --dont-prompt-for-dependencies

rm -rf temp1
rm -rf temp2
mkdir temp2
mkdir temp1
cd temp2
darcs init
cd ..
cd temp1
darcs init
echo foo > f
darcs record -Ax -alm foo1
echo bar > b
darcs rec -Ax -alm bar1
echo foo2 > f
darcs record -Ax -alm foo2
echo bar2 > b
darcs record -Ax -alm bar2
echo y | darcs push ../temp2 --dont-prompt-for-dependencies -p foo2 --dry-run -i > toto
#on the previous line, we get asked about foo2, and we take it
grep foo2 toto | wc -l | grep 2
#we don't get asked about foo1, but we take it anyway, so 
grep foo1 toto | wc -l | grep 1
#and we don't send bar
grep bar toto | wc -l | grep 0
cd ..
rm -rf temp1 temp2
