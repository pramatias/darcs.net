#!/usr/bin/env bash
## Test for issue1373 - check that --token-chars [^ \t] is allowed.
## While we're at it, check some other things *aren't* allowed.
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

rm -rf temp                     # Another script may have left a mess.
darcs init --repodir temp
replace () { darcs replace --repodir temp --token-chars "$1" x y; }
## These are not well-formed tokens.
not replace ''
not replace 'a'
not replace ']a['
not replace '[]'
not replace '[^]'
## These are well-formed, but allow tokens to contain whitespace.
not replace $'[ ]'
not replace $'[\t]'
not replace $'[\n]'
not replace $'[\r]'
not replace $'[\v]'
not replace $'[^ ]'
not replace $'[^\t]'
not replace $'[^\n]'
not replace $'[^\r]'
not replace $'[^\v]'
rm -rf temp                     # Clean up after ourselves.
