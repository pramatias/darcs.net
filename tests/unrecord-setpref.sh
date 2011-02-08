#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init

darcs setpref boringfile foobar

darcs whatsnew > correct
cat correct

darcs record -a -m 'boringfoobar'

darcs unrecord -a

darcs whatsnew > unrecorded
cat unrecorded

diff -u correct unrecorded

cd ..
rm -rf temp1
