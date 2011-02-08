#!/usr/bin/env bash
## Test for issue121 - amend-record --ask-deps
##
## Copyright (C) 2009 Ganesh Sittampalam
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
touch a
darcs add a
darcs rec --ignore-times -am 'add a'
(echo '1' ; echo '1' ; echo '1') > a
darcs rec --ignore-times -am 'patch X'
(echo '2' ; echo '1' ; echo '1') > a
darcs rec --ignore-times -am 'patch Y'
(echo '2' ; echo '1' ; echo '2') > a
darcs rec --ignore-times -am 'patch Z'

darcs obliterate --dry-run --patch 'patch Y' | not grep 'patch Z'

echo 'yy' | darcs amend-rec --ask-deps

darcs obliterate --dry-run --patch 'patch Y' | grep 'patch Z'
