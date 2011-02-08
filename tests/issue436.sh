#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
mkdir temp1
cd temp1
# this test fails in the darcs 1 format
darcs init --darcs-2
echo A > f
darcs add f
darcs record --ignore-times -a -m A
cd ..

darcs get temp1 temp2

cd temp1
echo C > f
darcs record --ignore-times -a -m A-C
cd ..

cd temp2
echo B > f
darcs record --ignore-times -a -m A-B
echo A > f
darcs record --ignore-times -a -m B-A
(darcs push -a || :) 2> push-result
grep "Refusing to apply" push-result
cd ..

rm -rf temp1 temp2
