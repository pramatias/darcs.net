#!/usr/bin/env bash
##
## Regression test for patch178
##
## Copyright (C) 2010 Alexey Levan
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
darcs init      --repo R        # Create our test repos.
cd R

if darcs move /non_existent_path/a /non_existent_path/b 2>&1 | grep 'bug'; then
  echo 'Not OK 1: darcs move causes a bug'
  exit 1
else
  echo 'OK 1'
fi

if darcs move /non_existent_path/a /non_existent_path/b /non_existent_path/c 2>&1 | grep 'Prelude.init: empty list'; then
  echo 'Not OK 2: darcs move causes an error'
  exit 1
else
  echo 'OK 2'
fi

if darcs annotate /non_existent_path/a 2>&1 | grep 'Pattern match failure'; then
  echo 'Not OK 3: darcs annotate causes an error'
  exit 1
else
  echo 'OK 3'
fi

cd ..
rm -rf R
