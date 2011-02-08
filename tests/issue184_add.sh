#!/usr/bin/env bash

. lib

# For issue184: recording files in directories that haven't explicity been added.

rm -rf temp1
mkdir temp1
cd temp1
darcs init
mkdir new
mkdir new/dir
touch new/dir/t.t
darcs add new/dir/t.t
darcs record -am test new/dir/t.t > log
not grep "don't want to record" log
cd ..

rm -rf temp1
