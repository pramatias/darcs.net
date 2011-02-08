#!/usr/bin/env bash
## Test for issue1224 - Attempting to darcs convert a repository
## which is already in darcs-2 format leads to inconsistent result
##
## Copyright (C) 2009 Tomas Caithaml
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

# this test is not relevant for other than darcs 2 repositories
grep darcs-2 $HOME/.darcs/defaults || exit 200

. lib

rm -rf R 
darcs init --repo R

echo File contents > R/file.txt
darcs add R/file.txt --repodir R
darcs record --name=add_file.txt --author=me --no-test -a --repodir R

# This should fail with repository already in darcs-2 format.
echo "I understand the consequences of my action" > ack
not darcs convert temp/repo-2 temp/repo-2-converted < ack

rm -rf R
