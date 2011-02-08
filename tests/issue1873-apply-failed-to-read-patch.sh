#!/usr/bin/env bash
## Test for issue1873 - apply should complain about the right
## patches if it says some are missing
##
## Copyright (C) 2010 Petr Rockai
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

rm -rf R S
mkdir R
darcs init --repo R

cd R
echo a > a
darcs rec -lam a
echo b > a
darcs rec -lam b
echo x > x
darcs rec -lam x
echo c > a
darcs rec -lam c
echo y > y
darcs rec -lam y
echo d > a
darcs rec -lam d
cd ..

darcs get R S
darcs unpull -p x -a --repo R
darcs send   -p x -a --repo S -o R/x.dpatch
darcs unpull -p y -a --repo R
not darcs apply --repo R R/x.dpatch 2>&1 | tee log

not grep '^  \* d' log # does not complain about an unrelated patch
    grep '^  \* y' log # complains about the offending one instead
