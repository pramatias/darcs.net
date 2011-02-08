#!/usr/bin/env bash
## Test for issue1898 - set-default mechanism
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
rm -rf R0 R1 R2 S               # Another script may have left a mess.
darcs init      --repo R0       # Create our test repos.

darcs get R0 R1
darcs get R0 R2
darcs get R0 S

cd S
# default to no-set-default
darcs push ../R1 > log
grep '/R0$' _darcs/prefs/defaultrepo
# notification when using no-set-default
grep "set-default" log
# set-default works
darcs push ../R1 --set-default > log
grep '/R1$' _darcs/prefs/defaultrepo
# no notification when already using --set-default
not grep "set-default" log
# no notification when already pushing to the default repo
darcs push > log
not grep "set-default" log
# no notification when it's just the --remote-repo
darcs push --remote-repo ../R1 > log
not grep "set-default" log
# but... notification still works in presence of remote-repo
darcs push --remote-repo ../R1 ../R2 > log
grep "set-default" log
cd ..
