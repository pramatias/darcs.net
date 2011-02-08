#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo foo > foo
darcs add foo

darcs record -a -m 'addfoo'

darcs remove foo

darcs whatsnew > correct
cat correct

darcs record -a -m 'rmfoo'

darcs unrecord -a --last 1

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1
