#!/usr/bin/env bash
## Test for issue1610 - another bug in get_extra problem
## This is an offshoot of the issue1609 test
##
## Copyright (C) 2009 Eric Kow <kowey@darcs.net>
## Copyright (C) 2009 Pascal Molli <momo54@gmail.com>
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

# this test is not relevant for darcs 2 repositories
(not grep darcs-2 $HOME/.darcs/defaults) || exit 200

rm -rf S1  S2  S3               # Another script may have left a mess.
darcs init      --repo S1       # Create our test repos.

cd S1
cat > f << END
1
2
3
4
5
END
darcs add f
darcs record -am init
cd ..

darcs get S1 S2
darcs get S1 S3

cd S1
cat > f << END
1
2
X
3
4
5
END
darcs record -am 'insert X before line 3'
cd ..

cd S2
cat > f << END
1
2
4
5
END
darcs record -am 'delete line 3'
cd ..

cd S3
cat > f << END
1
2
3
Y
4
5
END
darcs record -am 'insert Y after line 3'
cd ..

# please compare this with the issue1609 test
darcs pull -a --repo S1 S2
darcs pull -a --repo S1 S3

darcs pull -a --repo S2 S1
darcs pull -a --repo S2 S3
