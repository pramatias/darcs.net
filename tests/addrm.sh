#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
touch foo
darcs add foo
darcs record -a -m add_foo -A x
darcs remove foo
darcs record -a -m del_foo -A x
cd ../temp2
darcs init
darcs pull --all ../temp1
cd ..
rm -rf temp1 temp2

