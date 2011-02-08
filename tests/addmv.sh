#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x
darcs mv foo zig
darcs mv bar foo
darcs mv zig bar
darcs record -a -m swap_foo_bar -A x
cd ../temp2
darcs init
darcs pull -v -a ../temp1
cd ..
rm -rf temp1 temp2

