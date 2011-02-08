#!/usr/bin/env bash
## Test for issue1473 - check that darcs annotate works with and without
## repodir and with "." argument.  It should fail with the empty string as
## a single argument and without any arguments.
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

cd R
echo 'Example content.' > f
darcs record -lam 'Added f.'
darcs annotate .
darcs annotate f
not darcs annotate
not darcs annotate ''

cd ..
darcs annotate --repodir=R .
darcs annotate --repodir=R f
not darcs annotate --repodir=R
not darcs annotate --repodir=R ''
