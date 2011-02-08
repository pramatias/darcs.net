#!/bin/sh

set -ev

mkdir temp1
cd temp1
darcs init

echo a > foo
darcs add foo
darcs record -am addfoo

cd ..
darcs get temp1 temp2

cd temp2
echo B > foo
darcs record -am B

cd ../temp1
echo b > foo
darcs record -am b
echo c > foo
darcs record -am c
cd ../temp2
darcs pull -a

cat foo

grep b foo && exit 1
grep a foo && exit 1
grep B foo
grep c foo

echo C > foo
darcs record -am C

cd ../temp1
echo d > foo
darcs record -am d

cd ../temp2
darcs pull -a

cat foo

grep b foo && exit 1
grep a foo && exit 1
grep B foo && exit 1
grep c foo && exit 1
grep C foo
grep d foo
