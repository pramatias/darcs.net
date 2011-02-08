#!/usr/bin/env bash


. lib

if echo $OS | grep -i windows; then
    echo This test does not work under Windows
    exit 0
fi

# pull from not empty repo to empty repo
rm -rf temp1
mkdir temp1

cd temp1
darcs init
echo a > Aux.hs
not darcs add Aux.hs
darcs add --reserved-ok Aux.hs
echo b > foo
darcs add foo
darcs record -am 'two files'
not darcs mv foo com1
darcs mv --reserved-ok foo com1
cd ..

rm -rf temp1
