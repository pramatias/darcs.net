#!/usr/bin/env bash

. lib

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init
test -d _darcs
not darcs init
cd ..

# Some tests for the repodir flag
mkdir temp2
darcs init --repodir temp2
test -d temp2/_darcs

rm -rf temp1 temp2
