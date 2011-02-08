#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo foo > foo
darcs add foo

darcs record -a -m 'addfoo'

darcs obliterate -a

not darcs whatsnew

cd ..
rm -rf temp1
