#!/usr/bin/env bash

set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
darcs rec -m t1 -a -A tester
echo 1 >> foo
darcs what -s | grep -v No\ changes
darcs what -l | grep -v No\ changes
darcs what -sl | grep -v No\ changes

cd ..
rm -rf temp1

