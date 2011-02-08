#!/usr/bin/env bash
## Test for issue1446 - darcs amend-record -m foo destroys long description without warning
##
## Copyright (C) 2009  Dmitry Kurochkin
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
darcs init      --repo R        # Create our test repo.

touch R/f
darcs add f     --repo R
echo 'patch name' > R/patchinfo
echo 'patch description' >> R/patchinfo
darcs record -a --logfile=R/patchinfo --repo R
darcs changes   --repo R | grep 'patch name'
darcs changes   --repo R | grep 'patch description'

echo 'y' | darcs amend-record -p 'patch name' -m 'new name' --repo R
darcs changes   --repo R | grep 'new name'
darcs changes   --repo R | grep 'patch description'

echo content > R/f
echo 'another name' > R/patchinfo
echo 'another description' >> R/patchinfo
darcs record -a --logfile=R/patchinfo --repo R
darcs changes   --repo R | grep 'another name'
darcs changes   --repo R | grep 'another description'

echo 'y' | darcs amend-record -p 'another name' -m 'one more name' --repo R
darcs changes   --repo R | grep 'one more name'
darcs changes   --repo R | grep 'another description'

rm -rf R/                       # Clean up after ourselves.
