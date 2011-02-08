#!/usr/bin/env bash
## Test for issue1584 - darcs optimize --upgrade
##
## Copyright (C) 2009 Eric Kow <kowey@darcs.net>
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
darcs init      --repo R --old-fashioned # Create our test repos.
mkdir R/d/ R/e/                 # Change the working tree.
echo 'Example content.' >R/d/f
darcs record    --repo R -lam 'Add d/f and e.'
darcs mv        --repo R d/f e/  # Unrecorded change
darcs whatsnew  --repo R | grep 'move ./d/f'
darcs optimize  --repo R --upgrade
darcs check     --repo R
grep hashed R/_darcs/format
not grep darcs-2 R/_darcs/format
darcs whatsnew  --repo R | grep 'move ./d/f'
