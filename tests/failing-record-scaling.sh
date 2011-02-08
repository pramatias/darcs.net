#!/usr/bin/env bash
## Test for issueN - darcs record shouldn't access old inventories!
##
## Copyright (C) 2008  David Roundy
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
which strace || exit 200        # This test requires strace(1).

rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repo.
touch R/a-unique-filename
strace -eopen -oR/trace \
  darcs record  --repo R -lam 'A unique commit message.'
grep a-unique-filename R/trace
grep _darcs/hashed_inventory R/trace
not grep _darcs/inventories/ R/trace
rm -rf R                        # Clean up after ourselves.
