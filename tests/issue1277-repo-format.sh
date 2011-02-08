#!/usr/bin/env bash
## Test for issue1277 - repository format errors should be reported
## correctly (ie. not as some totally unrelated error)
##
## Copyright (C) 2010 Eric Kow
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
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
cd R
 darcs init --repo R2            # Protect the darcs darcs repo with R
 cd R2
 echo impossible >> _darcs/format
 echo 'Example content.' > f
 not darcs add f > log 2>&1
 grep "Can't understand repository format" log
 not darcs whatsnew > log 2>&1
 grep "Can't understand repository format" log
 not darcs init > log 2>&1
 grep "You may not run this command in a repository" log
 grep "Can't understand repository format" log
 cd ..
 not darcs whatsnew --repodir R2 > log 2>&1
 grep "R2 looks like a repository directory," log
 grep "Can't understand repository format" log
cd ..
