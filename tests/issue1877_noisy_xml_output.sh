#!/usr/bin/env bash
## Test for issue1877 - pull --dry-run --xml-output is noisy
##
## Copyright (C) 2010 Lele Gaifax <lele@nautilus.homeip.net>
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
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
echo 'Example content.' > foo
darcs record -lam 'Add foo.'
cd ..

cd S

# This does the right thing... at least until the second command is executed
darcs pull --dry-run --xml-output 2>&1 ../R | not grep "Would pull"

# This does not, never
darcs pull --summary --dry-run --xml-output 2>&1 ../R | not grep "Would pull"

# From now on, this fails too!
darcs pull --dry-run --xml-output 2>&1 ../R | not grep "Would pull"

cd ..
