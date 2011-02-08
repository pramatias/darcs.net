#!/usr/bin/env bash

set -ev

# Check that the right patches get unrecorded using --dont-prompt-for-dependencies

rm -rf temp1
mkdir temp1
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
darcs unrec --no-deps -p foo1
darcs changes -p foo --count | grep 2
#foo1 is depended upon, we don't unpull it
echo y | darcs unrec --dont-prompt-for-dependencies -p foo1
#on the previous line, we don't get asked about foo2.
darcs changes -p foo --count | grep 0
#yet, it is unrecorded.
darcs changes -p bar --count | grep 2
cd ..
rm -rf temp1
