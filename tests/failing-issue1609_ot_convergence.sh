#!/usr/bin/env bash
## Test for issue1609 - standard test for the TP2 property
## in the Operational Transformation literature.
##
## According to Wikipedia:
## For every three concurrent operations op1,op2 and op3 defined on the same
## document state, the transformation function T satisfies CP2/TP2 property
## if and only if:
## T(op_3, op_1 \circ T(op_2,op_1)) = T(op_3, op_2 \circ T(op_1,op_2)).
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

darcs pull --allow-conflicts -a --repo S1 S2
darcs pull --allow-conflicts -a --repo S2 S1

darcs pull --allow-conflicts -a --repo S1 S3
darcs pull --allow-conflicts -a --repo S2 S3

darcs pull --allow-conflicts -a --repo S3 S1

# TP2 is just fine for conflict resolution itself...
diff S1/f S2/f # no difference
diff S2/f S3/f # no difference

# But what about the conflict marking?
darcs mark-conflicts --repo S1
darcs mark-conflicts --repo S2
darcs mark-conflicts --repo S3
diff S1/f S2/f # no difference
diff S2/f S3/f # no difference
