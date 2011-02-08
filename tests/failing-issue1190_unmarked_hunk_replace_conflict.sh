#!/usr/bin/env bash
## Test for issue1190 - conflicts between HUNK and REPLACE are not
## marked by --mark-conflicts.
##
## Copyright (C) 2009  Trent W. Buck
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

. lib

rm -rf d e                      # Another script may have left a mess.
darcs init    --repo d/
printf %s\\n foo bar baz >d/f
darcs record  --repo d/ -lam f1
darcs get d/ e/
darcs replace --repo e/ --force bar baz f
darcs record  --repo e/ -lam replacement
printf %s\\n foo bar quux >d/f  # replace baz with quux
darcs record  --repo d/ -am f2

## There ought to be a conflict here, and there is.
darcs pull    --repo e/ d/ -a --mark-conflicts
## The file ought to now have conflict markers in it.
grep 'v v v' e/f
rm -rf d/ e/                    # Clean up after ourselves.
