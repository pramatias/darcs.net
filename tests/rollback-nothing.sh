#!/usr/bin/env bash

set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
date > file1
darcs add file1
darcs record -am "test"
rm file1
darcs record -am "rm"
echo yd | darcs rollback --last=1 -m 'oops' | grep 'No changes selected'
cd ..
rm -rf temp
