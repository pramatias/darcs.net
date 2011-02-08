#!/usr/bin/env bash
## Test for issue2017 - apply should gracefully handle tag missing
## from context (complain, not crash)
##
## Copyright (C) 2010 Eric Kow
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
darcs init      --repo R        # Create our test repos.

cd R
echo 'Example content.' > f
darcs record -lam 'Add f'
cd ..

# variant 0 - this passes trivially
darcs get R R0
darcs get R0 S0
darcs tag 's' --repo S0
darcs get S0 T0
cd T0
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
not darcs apply --repo R0 T0/foo.dpatch > log 2>&1
not grep bug log
grep missing log

# variant 1 - tag in shared context
darcs get R R1
darcs tag '1' --repo R1
darcs get R1 S1
darcs tag 's1' --repo S1
darcs get S1 T1
cd T1
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
# sanity check: should be able to cherry pick
darcs get R1 R1b
cd R1b
[ `darcs changes --count` -eq 2 ]
darcs pull ../T1 --match 'touch f' --all
[ `darcs changes --count` -eq 3 ]
cd ..
# the test: can't apply this due to incorrect context
not darcs apply --repo R1 T1/foo.dpatch > log 2>&1
not grep 'bug' log
grep missing log

# variant 2 - tag created after the fact
darcs get R  R2
darcs get R2 S2
darcs tag 's2' --repo S2
darcs get S2 T2
cd T2
echo 'More content.' > f
darcs record -lam 'Modify f'
darcs send -o foo.dpatch -a
cd ..
darcs tag '2'  --repo R2  # only tag after
not darcs apply --repo R2 T2/foo.dpatch > log 2>&1
not grep 'bug' log
grep missing log
