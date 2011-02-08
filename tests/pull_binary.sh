#!/usr/bin/env bash

# This test script, originally written by David Roundy and Ian Lynagh is in
# the public domain.
#
# This file is included as part of the Darcs test distribution,
# which is licensed to you under the following terms:
#
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

set -ev

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init
printf "%01048576d" 0 > foo
darcs record -l -a -A author -m xx
rm foo
darcs record -a -A author -m yy
cd ..

mkdir temp2
cd temp2
darcs init
echo yn | darcs pull --set-default ../temp1
rm foo
darcs pull -a
cd ..

rm -rf temp1 temp2
