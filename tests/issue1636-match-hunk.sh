#!/usr/bin/env bash
## Test for issue1636 - primitive match type: hunk
##
## Copyright (C) Kamil Dworakowski
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
rm -rf R
darcs init      --repo R        # Create our test repos.

cd R
echo 'first line' > f
darcs record -lam 'one'
echo 'second line' >> f
darcs record -am 'two'

darcs changes --match 'hunk first' > log
grep one log
not grep two log

darcs changes --match 'hunk line' > log
grep one log
grep two log

darcs changes --match 'hunk one' > log
not grep one log

# test searching for lines in the remove part of the hunk
echo 'first line' > f

darcs record -am 'three'
darcs changes --match 'hunk second' > log

grep three log
grep two log
not grep first log

