#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
echo zig > foo
darcs add foo
#sleep 1
darcs record -a -m add_foo -A x
cd ../temp2
darcs init
darcs pull -a ../temp1
cd ..
cmp temp1/foo temp2/foo
rm -rf temp1 temp2

