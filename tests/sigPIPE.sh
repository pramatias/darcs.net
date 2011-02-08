#!/usr/bin/env bash
set -ev

DARCS_EDITOR=echo
export DARCS_EDITOR

rm -rf temp1
mkdir temp1

cd temp1
darcs init

echo hello world > foo
darcs add foo
darcs record -a --author=me -m "First patch"
i=1
while test $i -le 100; do
  echo foo $i > foo;
  darcs record --ignore-times -a --author=me -m "Patch $i" > bar
  i=`expr $i + 1`
done
darcs tag --author=me -m 'silly tag'
i=1000
while test $i -le 1096; do
  echo foo $i > foo;
  darcs record --ignore-times -a --author=me -m "Patch $i" > bar
  i=`expr $i + 1`
done

darcs --version
darcs changes | head

cd ..
# rm -rf temp1
