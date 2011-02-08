#!/usr/bin/env bash
## Test that apply --skip-conflicts filters the conflicts
## appropriately.
##
## Copyright (C) 2009 Ganesh Sittampalam
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
rm -rf R S                      # Another script may have left a mess.

mkdir R
cd R
darcs init
echo 'foo' > foo
echo 'bar' > bar
darcs rec -lam 'Add foo and bar'
cd ..

darcs get R S

cd R
echo 'foo2' > foo
darcs rec -lam 'Change foo (2)'
echo 'bar2' > bar
darcs rec -lam 'Change bar (2)'
cd ..

cd S
echo 'foo3' > foo
darcs rec -lam 'Change foo (3)'
cd ..

cd R
darcs send -a ../S -o ../S/applyme.dpatch
cd ..

cd S
darcs apply --skip-conflicts applyme.dpatch
test `darcs changes --count` -eq 3
cd ..
