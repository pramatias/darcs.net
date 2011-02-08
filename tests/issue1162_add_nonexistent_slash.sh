#!/bin/sh

set -ev

not () { "$@" && exit 1 || :; }

rm -rf temp
mkdir temp
cd temp
darcs init
not darcs add a/ 2> err
cat err
grep 'does not exist (No such file or directory)' err
cd ..

rm -rf temp
