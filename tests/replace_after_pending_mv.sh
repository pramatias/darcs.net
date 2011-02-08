#!/usr/bin/env bash

set -ev

rm -fr temp1

mkdir temp1
cd temp1
darcs init

echo a b a b a b > A
darcs add A
darcs record --all --name=p1
darcs mv A B
if darcs replace a c B | grep Skipping; then
    exit 1
fi
cd ..

rm -fr temp1

