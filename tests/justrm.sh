#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
darcs record -a -m add_foo -A x
rm foo
darcs whatsnew
cd ..
rm -rf temp1

