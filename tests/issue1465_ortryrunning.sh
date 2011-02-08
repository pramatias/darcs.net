#!/usr/bin/env bash
## Test for issue1465 - ortryrunning should try RHS if AND ONLY IF the
## LHS wasn't found or wasn't executable.
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

. lib                  # Load some portability helpers.
darcs init      --repo R        # Create our test repo.

FAKE_EDITOR_HOME=`pwd`
cat <<FAKE > editor-good
echo fake > \$1
FAKE
chmod u+x editor-good

cat <<FAKE > editor-bad
exit 127
FAKE
chmod u+x editor-bad

cat <<FAKE > editor-gave-up
exit 1
FAKE
chmod u+x editor-gave-up

cat <<VI > vi
echo vi > \$1
VI
chmod u+x vi

cd R
mkdir d
unset TERM

DARCSDIR=$(dirname $(which darcs))
# the /dev/null stdin redirection is to make vi or the fallback editor just fail
DARCS_EDITOR=$FAKE_EDITOR_HOME/editor-good \
darcs record    -lam 'Initial commit.' --edit </dev/null &> log-1
darcs changes   > changes-1
darcs unrecord  -a
grep fake changes-1

# Bad editor: fall through to the next choice
DARCS_EDITOR=$FAKE_EDITOR_HOME/editor-bad \
PATH=.:$DARCSDIR \
darcs record    -lam 'Initial commit.' --edit </dev/null &> log-2
darcs changes   > changes-2
darcs unrecord  -a
grep "Initial" changes-2
egrep -i 'vi|emacs|nano|edit' log-2

# Normal failure (eg. user hit ^-C)
# If Darcs did the right thing, the output won't make any mention of
# the fallback editors.
DARCS_EDITOR=$FAKE_EDITOR_HOME/editor-gave-up \
darcs record    -lam 'Initial commit.' --edit </dev/null &> log-3
darcs changes   > changes-3
darcs unrecord  -a
grep "Initial" changes-3
not egrep -i 'not found|vi|emacs|nano|edit' log-3
