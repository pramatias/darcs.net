#!/usr/bin/env bash
## Test for issue1763 - When you pull in a conflicting hunk
## patch to a file with a non-ASCII name, and then pull from the same
## repo again, darcs crashes.
##
## Copyright (C) 2010  Reinier Lamers
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

abort_windows # FIXME! We should figure out what's going on here

rm -rf R S
darcs init --repo R

export LC_ALL=C

function check_consistent_filename {
  count=`darcs changes -v | grep 'hunk .*\.lisp' | sed -e 's/.*hunk //' -e 's/.lisp.*//' | sort | uniq | wc -l`
  test $count -eq 1
}

# Set up a repo with 3 patches to a non-ASCII-named file
cd R
touch kitöltés.lisp
darcs rec -l -a -m "Add"
echo hi >> kitöltés.lisp
darcs record -a -m "First edit"
cd ..

rm -rf S S2 S3
darcs get R S
darcs get R S2
darcs get R S3

cd R
echo hi >> kitöltés.lisp
darcs record -a -m "Second edit"
cd ..

# From another repo, pull the first two, edit, pull the third to get a
# conflict, pull again to get the crash
cd S
echo hello >> kitöltés.lisp
darcs record -a -m "My edit"
darcs pull -a ../R
darcs pull -a ../R
check_consistent_filename
cd ..

# duplicates
cd S2
echo hi >> kitöltés.lisp
darcs record -a -m "My duplicate edit"
darcs pull -a ../R
darcs pull -a ../R
check_consistent_filename
cd ..
