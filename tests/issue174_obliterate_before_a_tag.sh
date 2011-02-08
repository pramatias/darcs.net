#!/usr/bin/env bash

# issue174: behave better when we want to obliterate a patch that comes before a tag.
. lib

rm -rf temp1

mkdir temp1
cd temp1
darcs init
# Setup: Create a patch, a tag and another patch.
touch a.txt
darcs add a.txt
darcs record -am 'adding a' a.txt
darcs tag "first tag";
touch b.txt
darcs add b.txt
darcs record --ignore-time -a -m 'adding b' b.txt
darcs obliterate -p "adding a" -a > log
not grep "no patch" log
cd ..

rm -rf temp1
