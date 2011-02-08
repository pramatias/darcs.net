#!/usr/bin/env bash
## Test for issue1726 - Files whose names start with "_darcs" are considered
## boring, even if they don't match anything in the boring file, and even if
## you pass --boring to the command.
##
## Copyright (C) 2009 Daniel Dickison <danieldickison@gmail.com>
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
rm -rf R                        # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.

cd R


## First test expected failures with actual _darcs files/directories

function bad_add {
    filename="$1"
    touch "$filename"
    not darcs whatsnew -ls --boring
    not darcs whatsnew -ls
    not darcs add --boring "$filename"
}

touch _darcs/foo
bad_add _darcs
bad_add _darcs/
bad_add _darcs/foo
bad_add ./_darcs
bad_add ./_darcs/
bad_add ./_darcs/foo
bad_add "$PWD/_darcs"
bad_add "$PWD/_darcs/"
bad_add "$PWD/_darcs/foo"
bad_add "../${PWD##*/}/_darcs"
bad_add "../${PWD##*/}/_darcs/"
bad_add "../${PWD##*/}/_darcs/foo"



## Then test expected successes with files that aren't in _darcs

# Passing --boring should definitely succeed.
touch _darcsfoo
darcs whatsnew -ls --boring
darcs add --boring _darcsfoo
darcs record -am 'add _darcsfoo' _darcsfoo

# Without --boring, this tests the default boring file.
touch _darcsbar
darcs whatsnew -ls
darcs add _darcsbar
darcs record -am 'add _darcsbar' _darcsbar
