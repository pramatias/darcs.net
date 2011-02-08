#!/usr/bin/env bash


. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo 'test file 1' > foo
darcs record --look-for-adds --all -m"Patch 1"
echo 'test boringfile' > bar
darcs setpref boringfile bar
darcs record --look-for-adds --all -m"Patch 2"
echo 'test file 3' > baz
# there should be no -R
darcs record --look-for-adds --all -m"Patch 3" > ../temp2
cat ../temp2
not grep R ../temp2

cd ..

rm -rf temp1
rm -f temp2
