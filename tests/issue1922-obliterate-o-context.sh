#!/usr/bin/env bash
## Test for issue1922 - obliterate -o does not give the right context
##
## Copyright (C) 2010 Florent Becker
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

. lib                           # Load some portability helpers.
darcs init      --repo R        # Create our test repos.

rm -rf x
mkdir x
darcs init --repo x
echo a > x/a
darcs rec -lam a --repo x --ig
echo b > x/a
darcs rec -lam b --repo x --ig
echo a > x/b
darcs rec -lam a2 --repo x --ig
echo b > x/b
darcs rec -lam b2 --repo x --ig
echo c > x/a
darcs rec -lam c --repo x --ig

rm -f foo.dpatch
darcs unpull -a -o foo.dpatch --match 'name a2' --repo x --last 3
cat foo.dpatch
darcs chan --context --repo x
darcs apply foo.dpatch --repo x