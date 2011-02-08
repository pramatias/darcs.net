#!/usr/bin/env bash

## Tests for convert command based on previously checked results
## to generate new test material for this test,
## see bin/convert-writer.sh
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

. lib

grep old-fashioned $HOME/.darcs/defaults || grep hashed $HOME/.darcs/defaults || exit 200

runtest() {
    rm -rf temp
    mkdir temp
    cd temp

    mkdir repo
    cd repo
    darcs init
    darcs apply --allow-conflicts $TESTDATA/convert/darcs1/$1.dpatch
    cd ..
    echo 'I understand the consequences of my action' | darcs convert repo repo2
    mkdir empty-darcs2
    cd empty-darcs2
    darcs init --darcs-2
    cd ..
    cd repo2
    darcs send -a -o ../$1-darcs2.dpatch ../empty-darcs2
    cd ..
    diff -I'1 patch for repository ' -I'patches for repository ' -I'Oct 1' -u $TESTDATA/convert/darcs2/$1.dpatch $1-darcs2.dpatch
}

runtest simple
runtest twowayconflict
runtest threewayconflict
runtest threewayanddep
runtest threewayandmultideps
runtest resolution
runtest tworesolutions
