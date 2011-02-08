#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init

cd ../temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x
darcs mv foo zig
darcs mv bar foo
darcs mv zig bar
darcs record -a -m swap_foo_bar -A x
darcs send --author=me --output=funpatch --dont-sign -a ../temp2

gzip funpatch

cd ../temp2
darcs apply ../temp1/funpatch.gz
cd ..
cmp temp1/bar temp2/bar
rm -rf temp2

mkdir temp2
cd temp2
darcs init
darcs apply ../temp1/funpatch.gz
## Also test that "darcs apply" can accept a patch on stdin.
darcs obl -a
darcs apply < ../temp1/funpatch.gz
cd ..
cmp temp1/bar temp2/bar


rm -rf temp1 temp2

