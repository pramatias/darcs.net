#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
mkdir dir
echo zig > dir/foo
darcs add dir dir/foo
darcs record -a -m add_foo
# Create second repo inside the first
darcs init --repodir=temp2
cd  temp2
darcs pull -a ../../temp1
darcs changes -s | grep "A ./dir/foo"
# no differences
diff ../../temp1/dir/foo dir/foo
cd ..

rm -rf temp1
