#!/usr/bin/env bash

# This is a test for issue761, which pointed out that we didn't check the
# syntax of --match patterns until after having spent considerable time
# doing considerable work.  So here we construct a very invalid repository,
# and check that darcs fails *before* it notices that it's pulling from a
# bad repository.  Thus we verify that we aren't doing any work prior to
# checking the flags.

set -ev

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m 'add two files'
darcs tag -m tag
rm foo bar
darcs record -a -m 'rm two files'
darcs tag -m tag2

rm -rf _darcs/inventories/*
rm -rf _darcs/patches/*

cd ..

mkdir temp2
cd temp2
darcs init
! darcs pull --match 'foobar' ../temp1 2> error
cat error

grep foobar error

rm -rf temp1 temp2

