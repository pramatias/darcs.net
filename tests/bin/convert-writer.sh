#!/usr/bin/env bash

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

# This script is a convenient hack for generating test material
# for convert.sh.

# To generate new test material, move this into the tests
# directory temporarily, set up the test repo in the relevant
# section of this code, set the test name, then
# 'cabal test convert-writer.sh', then manually inspect the
# test data and/or play with the repo it made to be sure it's
# as wanted.

# Note that the script is deliberately setup to fail so we
# can inspect the output of darcs convert, and also get hold of
# the generated test repos easily for playing with.

. lib

# only run this once
grep darcs-2 $HOME/.darcs/defaults || exit 200

rm -rf temp
mkdir temp
cd temp

# set up the test repo here
mkdir repo
cd repo
darcs init --hashed
echo wibble > wibble
darcs rec -lam 'wibble'
cd ..

darcs get repo repoA
cd repoA
echo 'A' >> wibble
darcs rec -lam 'A'
cd ..

darcs get repo repoB
cd repoB
echo 'B' >> wibble
darcs rec -lam 'B'
cd ..

darcs get repoA repoAB
cd repoAB
darcs pull -a --allow-conflicts ../repoB
echo 'AB' >> wibble
darcs rec -lam 'AB'
cd ..

darcs get repo repoC
cd repoC
echo 'C' >> wibble
darcs rec -lam 'C'
cd ..

darcs get repoAB repoABC
cd repoABC
darcs pull -a --allow-conflicts ../repoC
echo 'ABC' >> wibble
darcs rec -lam 'ABC'
cd ..

darcs get repo repoD
cd repoD
echo 'D' >> wibble
darcs rec -lam 'D'
cd ..


cd repo
darcs pull -a --allow-conflicts ../repoABC
darcs pull -a --allow-conflicts ../repoD
cd ..

# end test repo setup: change the test name below too
testname=tworesolutions

echo 'I understand the consequences of my action' | darcs convert repo repo2

mkdir empty-hashed
cd empty-hashed
darcs init --hashed
cd ..

mkdir empty-darcs2
cd empty-darcs2
darcs init --darcs-2
cd ..

cd repo
darcs send -a -o $TESTDATA/convert/darcs1/$testname.dpatch ../empty-hashed
cd ..

cd repo2
darcs send -a -o $TESTDATA/convert/darcs2/$testname.dpatch ../empty-darcs2
cd ..

# fail so we can see the output
exit 1