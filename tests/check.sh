#!/usr/bin/env bash
## Ensure that "darcs check --test" succeeds when run in a complete
## repository with a dummy test that always succeeds.
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

. lib                           # Load some portability helpers.
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R
darcs setpref   --repo R test 'grep hello f'
not darcs record    --repo R -am 'true test' --test
darcs record    --repo R -am 'true test' --no-test

touch R/f
darcs record    --repo R -lam 'added foo' --no-test
darcs tag       --repo R -m 'got f?'

echo hello > R/f
darcs record    --repo R -lam 'hellofoo' --test
darcs chec      --repo R --test

rm -rf R                        # Clean up after ourselves.
