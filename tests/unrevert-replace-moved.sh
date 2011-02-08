#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo hello world > foo
darcs add foo

darcs record -a -m 'addfoo'

darcs replace hello goodbye foo

darcs revert -a

not darcs whatsnew

darcs mv foo bar

echo hello my good friends >> bar

darcs unrevert -a

darcs whatsnew > unrecorded
cat unrecorded

grep 'bar .* hello goodbye' unrecorded

cat bar
grep 'goodbye world' bar
grep 'goodbye my good friends' bar

cd ..
rm -rf temp1
