#!/usr/bin/env bash

set -ev

rm -fr temp1

mkdir temp1
cd temp1
darcs --version

darcs init

echo a b a b a b > A
darcs add A
if darcs replace a c A | grep Skipping; then
    exit 1
fi
cd ..

rm -fr temp1

