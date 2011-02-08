#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
printf "File contents" > foo
darcs add foo
darcs record -a -m 'foo' foo
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16; do cat foo foo > foo2; mv foo2 foo; done
darcs what
cd ..
