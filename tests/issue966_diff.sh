#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp; cd temp
darcs init
echo "aaa diff" > file
darcs add file
darcs record -a -m "aaa"
echo "bbb diff" >> file
darcs record -a -m "bbb"

darcs diff --patch "aaa" | grep "aaa diff"
darcs diff --patch "bbb" | grep "bbb diff"

darcs tag release-1
darcs optimize

echo "ccc diff" >> file
darcs record -a -m "ccc"
darcs diff --patch "ccc" | grep "ccc diff"

# here is where we have a problem
darcs diff --patch "aaa" | grep "aaa diff"
darcs diff --patch "bbb" | grep "bbb diff"
