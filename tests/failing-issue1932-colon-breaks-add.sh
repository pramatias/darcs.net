#!/usr/bin/env bash
## Test for issue1932 - "darcs add -qr ." should not break on files with colons
##
## Copyright(C) 2010 Dmitry Astapov
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
mkdir -p R                      # Create our test repo.

cd R
darcs init
darcs --version

# Colons could be in repo names and in file name.
# Colon in repo name is an indication of special case - remote repo.
# Colon in the file could be there under unix and requires no special treatment.

# Repo name with ':' is either scp repo or http repo.
# Let's check scp repo first.
( darcs get user@host:path || true ) > log 2>&1
[ -n "$(fgrep  '(scp) failed to fetch' log)" ]

# HTTP repo
( darcs get http://www.bogus.domain.so.it.will.surely.fail.com || true ) > log 2>&1
[ -n "$(fgrep  'CouldNotResolveHost' log)" ]

# All following files should not be added unless "--reserved-ok" is specified,
# but should be added with "--reserved-ok" just fine
mkdir funny
touch funny/0401:19d2
touch funny/c:src
touch funny/c:\\src
touch funny/user@host:path 
touch funny/droundy@host:    
touch funny/host:path

# Try to add those files. None should be added, darcs should not fail
darcs add -qr funny
darcs wh -l > log 2>&1

# Check that darcs didn't drop dead as 2.4.4 does
[ -z "$(fgrep 'fromJust: Nothing' log)" ]

# Check that no funny files were added
[ -z "$(grep  '^A \./funny/.' log)" ]

# Now let's allow colons and add those files
darcs add --reserved-ok -qr funny 
darcs wh -l > log 2>&1

# This should add all those files
[ -n "$(grep  '^A \./funny/0401:19d2' log)" ]
[ -n "$(grep  '^A \./funny/c:src' log)" ]
[ -n "$(grep  '^A \./funny/c:\\src' log)" ]
[ -n "$(grep  '^A \./funny/user@host:path' log)" ]
[ -n "$(grep  '^A \./funny/droundy@host:' log)" ]
[ -n "$(grep  '^A \./funny/host:path' log)" ]
