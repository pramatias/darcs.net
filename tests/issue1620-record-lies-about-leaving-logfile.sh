#!/usr/bin/env bash
## Test for issue1620 - record does not really leave logfile
##                      after a failure
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

set -ev
. lib
rm -rf R; mkdir R; cd R
export DARCS_EDITOR="echo 'a log' > "
darcs init
darcs setpref test false
echo yy| not darcs record -m foo -a --edit-long-comment --test 2> out

# the msg has the format: "Logfile left in filenamehere."
LOGFILE=`grep "Logfile left in" out | sed "s/Logfile left in //" | sed s/.$//`
test -e "$LOGFILE"
grep 'a log' $LOGFILE

rm out
cd ..
rm -rf R/
