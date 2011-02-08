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

if grep old-fashioned .darcs/defaults; then
format=old-fashioned-inventory
patchtype=darcs-1
elif grep darcs-2 .darcs/defaults; then
format=darcs-2
patchtype=darcs-2
elif grep hashed .darcs/defaults; then
format=hashed
patchtype=darcs-1
else exit 200; fi

rm -rf split--${format}
gunzip -c $TESTDATA/split--${format}.tgz | tar xf -
cd split--${format}
darcs check
cd ..

# .dpatch tests: .dpatch files for darcs 2 split patches were
# broken in darcs 2.5 (and probably always broken) so we don't
# bother to test them.

if [ ${patchtype} != darcs-1 ] ; then exit 0 ; fi

rm -rf temp
mkdir temp
cd temp
darcs init
darcs apply $TESTDATA/split--${patchtype}.dpatch
cd ..

rm -rf temp2
mkdir temp2
cd temp2
darcs init
darcs apply $TESTDATA/split2--${patchtype}.dpatch
cd ..
