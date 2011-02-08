#!/usr/bin/env bash
## Test for issue1739 - "Char.intToDigit: not a digit" in darcs changes 
##
## Copyright (C) 2010  Reinier Lamers
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

. lib

# First, try to see if character set is UTF-8. If we can't find out or if it
# isn't, we skip this test.
if ! which locale ; then
    echo "no locale command"
    exit 200 # skip test
fi

charmap=`locale charmap`
if [ $? -ne 0 ]; then
    echo "couldn't determine locale character set"
    exit 200 # skip test
fi

if [ "$charmap" != "UTF-8" ]; then
    echo "locale character set is not UTF-8, skipping"
    exit 200
fi

# we want escaping, otherwise output of non-ASCII characters is unreliable
export DARCS_DONT_ESCAPE_ANYTHING=0

rm -rf R
mkdir R
cd R
darcs init

echo garbelbolf > aargh
darcs add aargh
echo -e '\xe2\x80\x9e\x54\x61\x20\x4d\xc3\xa8\x72\x65\xe2\x80\x9d' > message.txt
darcs record --logfile=message.txt -A 'Petra Testa van der Test <test@example.com>' -a > rec.txt
darcs changes > log.txt
cat log.txt
grep '<U+201E>' log.txt
grep '<U+201D>' log.txt
grep '<U+00E8>' log.txt

# locale should not matter
LC_ALL=C darcs changes > log.txt
grep '<U+201E>' log.txt
grep '<U+201D>' log.txt
grep '<U+00E8>' log.txt

cd ..

