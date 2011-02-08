#!/usr/bin/env bash
## Test for rollback --no-record
##
## Copyright (C) 2010 Florent Becker
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
darcs init      --repo S

cd R
mkdir d e
echo 'Example content.' > d/f
darcs record -lam 'Add d/f and e.'
darcs mv d/f e/
darcs replace Example Kestatepa e/f
echo "Blih blah bloh" >> e/f
darcs record -am 'Some changes.'

cat <<EOF >> e/f

Kestatepa

EOF

cat e/f
darcs record -am 'one more line in f'
darcs changes --context > context
darcs rollback -a -p 'Some changes' -m 'rollback'
darcs send -a -o rollback.dpatch --context context ../S
darcs obliterate -a -p rollback
darcs rollback -a -p 'Some changes' --no-record
darcs record -am 'rollback'
darcs send -a -o rollback2.dpatch --context context ../S

#check the effects
not ls e/f
ls d/f
grep Example d/f
not grep "Blih" d/f

#check the content of the patches
#"diff rollback.dpatch rollback2.dpatch" fails because of
#the order of the hunks.
grep "replace .* Kestatepa Example" rollback2.dpatch
grep "move ./e/f ./d/f" rollback2.dpatch
grep -e "-Blih blah bloh" rollback2.dpatch

