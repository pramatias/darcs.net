#!/usr/bin/env bash

# For issue588, "amend-record --look-for-adds end up with two "addfile" entries"

set -ev

rm -rf temp1
darcs init --repodir temp1

# Setup f with contents foo.
echo foo > temp1/f
darcs add --repodir temp1 f
darcs rec --repodir temp1 -am p1

# Remove f, and amend p1, but only the hunk not the rmfile.
# Here we use look-for-adds to trigger the bug
rm temp1/f
echo yyd | darcs amend-record --repodir temp1 --look-for-adds

darcs changes --repodir temp1 --last 1 -v

echo show the buggy pending
cat temp1/_darcs/patches/pending

echo bar > temp1/f
echo y | darcs amend-record --repodir temp1 --all

darcs changes --repodir temp1 --last 1 -v

darcs check --repodir temp1 --no-test

rm -rf temp1
