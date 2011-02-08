#!/usr/bin/env bash
##
## Copyright (C) 2009  Roman Plasil
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
darcs init      --repo R        # Create our test repos.

cd R
mkdir d e                       # Change the working tree.
mkdir d/f d/f/g
echo 'Example content.' > d/f/1.txt
echo 'Example content.' > d/f/g/2.txt
echo 'Example content.' > e/3.txt

darcs add -r .
darcs wh -s > before.lst
grep -i d before.lst 
grep -i e before.lst
grep -i d/f before.lst

darcs remove -r d
darcs wh -s > after.lst
not grep -i d after.lst 
not grep -i d/f after.lst
not grep -i d/f/g after.lst
not grep -i d/f/g/2.txt after.lst
not grep -i d/f/1.txt after.lst

cd ..
