#!/usr/bin/env bash
set -ev

# RT#544 using context created with 8-bit chars;
rm -rf temp1

mkdir temp1

cd temp1
darcs init
touch foo
darcs record -la -m 'add\212 foo' | grep 'Finished record'
darcs changes --context >context
date > foo
darcs record -a -m 'date foo' | grep 'Finished record'
darcs send -a -o patch --context context . | grep 'Wrote patch to'
cd ..
rm -rf temp1
