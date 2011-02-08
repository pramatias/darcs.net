#!/usr/bin/env bash

set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo

# Check that prehook runs
darcs whatsnew -s --prehook 'touch prehook-ran'
test -f prehook-ran
rm prehook-ran

# Check that --prehook works with defaults...
echo ALL --prehook touch prehook-ran > _darcs/prefs/defaults
darcs whatsnew -s
test -f prehook-ran
rm prehook-ran


echo Successful.

cd ..
rm -rf temp1
