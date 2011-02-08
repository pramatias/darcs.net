#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo hello world > foo
darcs add foo
darcs record -a -m add -A x
echo goodbye world >> foo
cp foo bar
darcs revert -a
darcs show contents foo | cmp foo -

darcs unrevert -a
cmp foo bar

cd ..
rm -rf temp1

