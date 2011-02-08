#!/usr/bin/env bash
set -ev

rm -rf foo1 foo2
mkdir foo1 foo2
cd foo1
darcs init
echo zig > foo
darcs add foo
sleep 1
darcs record -a -m add_foo -A x
#sleep 1
echo zag >> foo
darcs record --ignore-time -a -m mod_foo -A x
cd ../foo2
darcs init
darcs pull -a ../foo1
cd ..
cmp foo1/foo foo2/foo
rm -rf foo1 foo2

