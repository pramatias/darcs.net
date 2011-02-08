#!/usr/bin/env bash
## Test for issue1392 - .authorspelling processing.
##
## Copyright (C) 2009 Tomas Caithaml
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
rm -rf R                       # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
cd R

echo 'Bad\, Jr. <a@a.net>, Foo' > .authorspellings
echo 'Example content.' > f
darcs record -lam 'Add f.' -A 'Foo'
darcs show authors | grep -q 'Bad, Jr\. <a@a.net>' 

echo 'Bad\, Jr. <a@a.net>,  ^Foo\, Jr\..*$' > .authorspellings
echo 'Ex. cont.' > f
darcs record -lam 'Change f.' -A 'Foo, Jr. <a@b.net>'
darcs show authors | tee output.txt | grep -q 'Bad, Jr\. <a@a.net>'

cd ..

#--rm -rf R
