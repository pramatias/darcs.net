#!/usr/bin/env bash

## Test for correct handling of Darcs v1 patches with nested { }
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

grep old-fashioned $HOME/.darcs/defaults || grep hashed $HOME/.darcs/defaults || exit 200

rm -rf braced
gunzip -c $TESTDATA/braced.tgz | tar xf -
cd braced
darcs check
cd ..

rm -rf empty
mkdir empty
cd empty
darcs init
darcs apply $TESTDATA/braced.dpatch
cd ..

cd braced
darcs pull -a ../empty | grep 'No remote changes to pull in'

cd ../empty
darcs pull -a ../braced | grep 'No remote changes to pull in'