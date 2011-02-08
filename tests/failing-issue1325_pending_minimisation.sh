#!/usr/bin/env bash
## Test for issue1325 - hunk patches interfere with pending patch
## minimisation
##
## Copyright (C) 2009 Marco Túlio Gontijo e Silva, Eric Kow
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
darcs init   --repo=R
darcs init   --repo=S

# this is expected to pass regardless of issue1325
# and is here to provide contrast
cd R
touch file
darcs add file
darcs record -am ' file'
mkdir b
darcs add b
darcs mv  file b
rm -r b
darcs whatsnew | not grep adddir
cd ..

# this is/was the failing part of issue1325
cd S
echo file > file                # we need a hunk to make this interesting
darcs add file
darcs record -am ' file'
mkdir b
darcs add b
darcs mv  file b
rm -r b
darcs whatsnew | not grep adddir
cd ..
