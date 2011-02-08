#!/usr/bin/env bash
## Test for gzcrcs command - check and repair corrupted CRCs on
## compressed files
##
## Copyright (C) 2009 Ganesh Sittampalam
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

# need to do this before loading lib as that sets -e
darcs gzcrcs --help > /dev/null
if [ $? == 2 ] ; then echo gzcrcs not supported by this darcs ; exit 0 ; fi

. lib                  # Load some portability helpers.
rm -rf maybench-crc
gunzip -c $TESTDATA/maybench-crc.tgz | tar xf -
cd maybench-crc
not darcs gzcrcs --check
darcs gzcrcs --repair
darcs gzcrcs --check
cd ..
rm -rf maybench-crc
