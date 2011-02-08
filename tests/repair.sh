#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs initialize
echo ALL ignore-times >> _darcs/prefs/defaults
echo A1 > foo
mkdir d
echo A2 > d/bar
darcs add foo
darcs add d
darcs add d/bar
darcs record -a -m AA -A x
echo B > foo
darcs record -a -m BB -A x
echo C > foo
darcs record -a -m CC -A x

for i in _darcs/pristine*; do
  echo Empty the pristine directory: $i
  rm -rf $i
  mkdir $i
done

darcs repair

cd ..
rm -rf temp1
