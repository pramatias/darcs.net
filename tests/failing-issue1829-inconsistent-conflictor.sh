#!/usr/bin/env bash
## Leads darcs into creating an inconsistent conflictor.
## Public Domain, 2010, Ganesh Sittampalam, Ian Lynagh and Petr Rockai

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
. lib

rm -rf r1 r2

mkdir r1
cd r1
darcs init
echo Line BB    >  file
darcs add file
darcs rec -a -m "Main patch 1"
echo Line DDDD  >> file
darcs rec -a -m "Main patch 2"
echo Line A     >  file
echo Line BB    >> file
echo Line CCC   >> file
echo Line DDDD  >> file
echo Line EEEEE >> file
darcs rec -a -m "Main patch 3"
cd ..

mkdir r2
cd r2
darcs init
darcs pull -a -p "Main patch 1" ../r1
echo Line TTTTTTT   >> file
darcs rec -a -m "Alternate patch 1"
darcs pull -a -p "Main patch 2" ../r1
darcs revert -a
echo Line XXXXXXXXX >> file
darcs rec -a -m "Alternate patch 2"
echo Line XXXXXXXXX >  file
darcs rec -a -m "Alternate patch 3"
cd ..

cd r1
darcs pull -a ../r2
