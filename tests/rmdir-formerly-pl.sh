#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
mkdir foo
echo hello world > foo/bar
echo hello world > foo/baz
mkdir foo/dir
darcs add foo foo/bar foo/dir foo/baz
darcs record -a -m add
rm -rf  foo
darcs show files --no-pending --no-dir > log
grep 'foo/baz' log
grep 'foo/bar' log
darcs show files --no-pending --no-fil > log
grep 'foo/dir' log
grep 'foo$'    log
# now without...
darcs record -a -m del
darcs show files --no-pending --no-dir > log
not grep 'foo/baz' log
not grep 'foo/bar' log
darcs show files --no-pending --no-fil > log
not grep 'foo/dir' log
not grep 'foo$'    log
cd ..

rm -rf temp1
