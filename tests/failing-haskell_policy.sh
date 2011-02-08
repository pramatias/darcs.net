#!/usr/bin/env bash
## This is a pseudo-test that runs tweaked hlint on the source code.
##
## Copyright (C) 2009 Petr Rockai
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

explain() {
    echo >&2
    echo "## It seems that hlint has found errors. This usually means that you" >&2
    echo "## have used a forbidden function. See contrib/darcs-errors.hlint for" >&2
    echo "## explanation. Please also disregard any possible parse errors." >&2
}

trap explain ERR

hlint >& /dev/null || exit 200 # skip if there's no hlint

wd="`pwd`"
cd ..
hlint --hint=contrib/darcs-errors.hlint src
