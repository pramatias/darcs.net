#!/usr/bin/env bash
## Test for keeping the repository in consitent state in case
## of a test failure on amend-record. The bug was almost introduced
## when trying to fix issue 1406.
##
## Copyright (C) 2009 Kamil Dworakowski
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
set -ev
rm -rf R
mkdir R
cd R
darcs init
# first patch: new file A
touch A
darcs add A
darcs record -a -m 'A'
# second patch: mv A to B
darcs mv A B
darcs record -a -m 'move'
# third patch: modify B
echo "content" > B
darcs record -a -m 'add content'

# amending 'move' results in commuting 'move' patch
# to the end for removal. The commute changes the "add content"
# patch to modify A instead of B. But the amend is interrupted
# because of test failure. Check the consitency after the operation.
darcs setpref test false
echo yy | not darcs amend -p move --test
darcs check

# Note: Amend-record in case of test failure is broken as described in issue1406,
# though when trying to fix it I almost managed to break darcs even more.
# This test is to guard against such regressions in the future.

cd ..
rm -rf R
