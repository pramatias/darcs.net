#!/usr/bin/env bash
. lib

# Some tests for 'darcs whatsnew '

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch look_summary.txt
# --disable works on command line
not darcs whatsnew -sl --disable 2> log
grep -i disable log
# --disable works from defaults
echo 'whatsnew --disable' > _darcs/prefs/defaults
not darcs whatsnew -sl 2> log
grep -i disable log
cd ..

rm -rf temp1
