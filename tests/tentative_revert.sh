#!/usr/bin/env bash
## Test for clearing tentative state after a failed transaction
##
## Copyright (C) 2009  Kamil Dworakowski
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

set -ev
. lib
rm -rf R
darcs init      --repo R
touch R/foo
darcs record    --repo R -lam 'foo'
touch R/bar
darcs record    --repo R -lam 'bar'
echo "this change should stay uncommitted" >> R/foo
darcs setpref   --repo R test false
echo 'y' | not darcs amend --repo R -am 'change everything' R/foo --test
darcs setpref   --repo R test true

# if tentative state was not cleared, the previous changes
# from failed transaction would piggy back on the next
echo "xx" >> R/bar
echo 'y' | darcs amend     --repo R -am 'bar2' R/bar --test

# should have uncommitted changes
darcs wh      --repo R > changes
grep "this change should stay uncommitted" changes
