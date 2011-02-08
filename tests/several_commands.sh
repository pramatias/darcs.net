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
darcs diff
darcs diff -u
darcs whatsnew
darcs whatsnew --summary
echo y | darcs revert -a
darcs show contents foo | cmp foo -
mkdir d
darcs add d
darcs record -a -m 'add dir' -A x
rmdir d
darcs revert -a d
cd ..
rm -rf temp1

