#!/usr/bin/env bash
## Test for issue1300 - record --delete-file should only delete
## after a successful record
##
## Copyright (C) 2009 Eric Kow
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
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
cd R
touch f
darcs add f
# no test
touch log
echo 'no test' > f
darcs record -am f --logfile log --delete-logfile
test ! -e log
# passing test
touch log
darcs setpref test 'exit 0'
echo 'test pass' > f
darcs record -am f --test --logfile log --delete-logfile
test ! -e log
# failing test
touch log
darcs setpref test 'exit 1'
echo 'test fail' > f
not darcs record -am g --test --logfile log --delete-logfile
test -e log # should *not* be deleted
cd ..
