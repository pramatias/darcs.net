#!/usr/bin/env bash
## -*- coding: utf-8 -*-
## Test for issue1442 - if we disable Darcs escaping and don't change
## our encoding, the bytes in a filename should be the same bytes that
## "darcs changes -v" prints to the right of "addfile" and "hunk".
##
## Copyright (C) 2009  Trent W. Buck
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

rm -rf R                        # Another script may have left a mess.

## Use the first UTF-8 locale we can find.
export LC_ALL=$(locale -a | egrep 'utf8|UTF-8' | head -1)
export DARCS_DONT_ESCAPE_ANYTHING=True

## If LC_ALL is the empty string, this system doesn't support UTF-8.
if test -z "$LC_ALL"
then exit 1
fi

darcs init      --repo R
echo '首頁 = א₀' >R/'首頁 = א₀'
darcs record    --repo R -lam '首頁 = א₀' '首頁 = א₀'
darcs changes   --repo R -v '首頁 = א₀' >R/log
#cat R/log                      # Show the humans what the output was.
grep -c '首頁 = א₀' R/log >R/count
echo 5 >R/expected-count
cmp R/count R/expected-count    # Both count files should contain "5\n".
rm -rf R                        # Clean up after ourselves.
