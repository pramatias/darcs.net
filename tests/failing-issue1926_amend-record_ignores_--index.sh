#!/usr/bin/env bash
## Test for issue1926 - amend-record --index
##
## Copyright (C) 2010 Iago Abal
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
#darcs init      --repo S

cd R
echo 'Example content 1.' > f
darcs record -lam 'Add file f'
echo 'Example content 2.' > g
darcs record -lam 'Add file g'
darcs put ../S                  # S as a copy of R
echo "y" | darcs amend-record --index=2 -m 'A new file f'   # Since --index is ignored this command works in
                                                            # interactive mode, and the amended patch is the one
                                                            # with index 1.
darcs changes > changes
cd ..

cd S
echo "y" | darcs amend-record -p 'Add file f' -m 'A new file f'  # -p works as expected and amends the right patch.
darcs changes > changes
cd ..

diff R/changes S/changes        # Shows us the differences between both 'darcs changes'.
