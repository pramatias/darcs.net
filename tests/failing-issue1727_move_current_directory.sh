#!/usr/bin/env bash
## Test for issue1727 - darcs move . target' fails as an attempt to
## write an 'invalid pending.
##
## Copyright (C) 2009 Sean Erle Johnson 
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

# Create repository R
darcs init      --repo R        # Create the test repo.
cd R

mkdir d                         # Change the working tree.

# darcs move empty current directory to existing directory d
darcs move . d

# darcs move empty current directory to non-existing directory e
darcs move . e

# Make file to be copied
echo 'main = putStrLn "Hello World"' > hello.hs

# darcs move non-empty current directory to existing directory d
darcs move . d

# darcs move non-empty current directory to non-existing directory e
darcs move . e
