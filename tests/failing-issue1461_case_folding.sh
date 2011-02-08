#!/usr/bin/env bash
## Test for issue1461 - patches to files whose names only differ by
## case can be wrongly applied to the same file in the working directory.
##
## Copyright (C) 2009 Eric Kow
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

touch casetest
test -e CASETEST || exit 200

rm -rf lower upper joint        # Another script may have left a mess.
mkdir lower upper

cd lower
darcs init
cat > a << EOF
1
2
3
EOF
darcs add a
darcs record -am 'lower init a'
cd ..

cd upper
darcs init
cat > A << EOF
1
2
3
EOF
darcs add A
darcs record -am 'upper init A'
cd ..

darcs get lower joint
cd joint
darcs pull -a ../upper
cd ..

cd lower
cat > a << EOF
one lower
2
3
EOF
darcs record -am 'lower modify'
cd ..

cd upper
cat > A << EOF
1
2
three upper
EOF
darcs record -am 'upper modify'
cd ..

cd joint
darcs pull ../lower -a
darcs pull ../upper -a
grep one a && not grep three a
grep three A && not grep one A
cd ..
# clean up after ourselves
rm -rf lower upper joint
