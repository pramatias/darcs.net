#!/usr/bin/env bash

set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init

date > foo
darcs record -a -m add_foo -A x --look-for-adds

# add a test file
echo 'test ! -e foo' > test.sh
darcs add test.sh
darcs record -a -m add_test

darcs setpref test 'ls && bash test.sh'
darcs record -a -m settest -A x --no-test

darcs mv foo bar
darcs record --debug -a -m mvfoo -A x

darcs check

cd ..
rm -rf temp1

