#!/usr/bin/env bash

## Test that we produce exactly correct output when sending v2 patches
##
## Copyright (C) 2010 Ganesh Sittampalam
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib                  # Load some portability helpers.

grep darcs-2 $HOME/.darcs/defaults || exit 200

rm -rf empty
mkdir empty
cd empty
darcs init
cd ..

rm -rf repo
gunzip -c $TESTDATA/simple-v2.tgz | tar xf -
cd repo
darcs send -o repo.dpatch -a ../empty

day=$(grep "<.*@.*>" $TESTDATA/simple-v2.dpatch | head -n 1 | cut -f1-3 -d' ')
diff -u -I'1 patch for repository ' -I'patches for repository ' -I"$day" $TESTDATA/simple-v2.dpatch repo.dpatch
cd ..

# context-v1 tests that we are including some context lines in hunk patches
rm -rf repo
gunzip -c $TESTDATA/context-v2.tgz | tar xf -
cd repo
darcs send -o repo.dpatch -a ../empty

day=$(grep "<.*@.*>" $TESTDATA/context-v2.dpatch | head -n 1 | cut -f1-3 -d' ')
diff -u -I'1 patch for repository ' -I'patches for repository ' -I"$day" $TESTDATA/context-v2.dpatch repo.dpatch
cd ..
