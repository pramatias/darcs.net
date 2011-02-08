#!/usr/bin/env bash
## Test for issue1363 - mark-conflicts should report that there is a
## conflict to mark if apply/push say there are
##
## Copyright (C) 2010 Eric Kow
## Copyright (C) 2009 Thorkil Naur
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
rm -rf R S T                    # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S
darcs init      --repo T

cd R
 touch f.txt
 darcs add f.txt
 darcs record -am "Empty f.txt"
 echo f.txt contents >> f.txt
 darcs record -am "Contents of f.txt"
cd ..

cd S
 echo yn | darcs pull ../R
 rm f.txt
 darcs record -am 'Remove f.txt in S'
cd ..

cd T
 echo ynn | darcs pull ../R
 rm f.txt
 darcs record -am 'Remove f.txt in T'
 not darcs push -a ../R > log # should fail because of conflict
 grep "There are conflicts" log
cd ..

cd R
  darcs pull -a ../S
  darcs revert -a
  darcs pull -a ../T
  echo y | darcs mark-conflicts > log
  not grep "No conflicts" log
cd ..
