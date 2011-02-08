#!/usr/bin/env bash

# Some tests for the output of changes when combined with move.

. lib

rm -rf temp1

mkdir temp1
cd temp1
darcs init
date > foo
darcs add foo
darcs record -m 'add foo' -a
mkdir d
darcs add d
darcs record -m 'add d' -a
darcs mv foo d
darcs record -m 'mv foo to d' -a
darcs mv d directory
darcs record -m 'mv d to directory' -a
echo 'How beauteous mankind is' > directory/foo
darcs record -m 'modify directory/foo' -a
darcs changes directory/foo > log
grep 'add foo' log
grep 'mv foo to d' log
echo 'O brave new world' > directory/foo
# darcs should also take unrecorded moves into account
darcs mv directory/foo directory/bar
darcs changes directory/foo > log
grep 'mv foo to d' log
echo 'That has such people in it' > directory/foo
darcs add directory/foo
darcs record -m 'mv foo then add new foo' -a
darcs annotate directory/bar > log
grep 'How beauteous mankind is' log
grep 'O brave new world' log
cd ..

rm -rf temp1
