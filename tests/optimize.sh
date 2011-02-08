#!/usr/bin/env bash
set -ev

# tests for "darcs optimize"

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
darcs record -a -m add_foo
darcs optimize --reorder-patches| grep -i "done optimizing"
cd ..

rm -rf temp1
