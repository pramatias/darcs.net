#!/usr/bin/env bash
## Test for issue70 - unrecorded (pending) changes to preferences
## should coalesce; if you set the same preference more than once in
## the same patch, only the last change should actually be recorded.
##
## Copyright (C) 2005  Eric Kow
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

rm -rf temp1
mkdir temp1
cd temp1
darcs init
darcs setpref predist apple
darcs setpref predist banana
darcs setpref predist clementine
darcs record -a  -m manamana
darcs changes --verbose > log
not grep apple log
not grep banana log
grep clementine log
cd ..
rm -rf temp1

# not sure what i'm going for here - if coalescing happens strictly
# before commuting, no problem, but what if patches are commuted
# before coalescing?
mkdir temp1
cd temp1
darcs init
darcs setpref predist apple
darcs setpref predist banana
darcs setpref predist apple
darcs setpref predist clementine
darcs setpref predist banana
darcs record -a  -m manamana
darcs changes --verbose > log
not grep apple log
not grep clementine log
grep banana log
cd ..
rm -rf temp1
