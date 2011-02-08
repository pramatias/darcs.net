#!/usr/bin/env bash
## Test for issue1871 - darcs record f, for f a removed file should work
##
## Public domain - 2010 Petr Rockai

. lib                           # Load some portability helpers.
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.

cd R
echo 'Example content.' > f
darcs rec -lam "first"
rm -f f
echo n | darcs record f 2>&1 | tee log
not grep "None of the files" log
cd ..
