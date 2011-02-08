#!/usr/bin/env bash
## Test for issue1599 - 'Automatically expire unused caches'
##
## Copyright (C) 2010  Adolfo Builes
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

skip-formats old-fashioned

darcs init --repo R
cd R
echo a > a
darcs rec -lam a
cd ..

serve_http
cat <<SOURCES > fake-sources
repo:$baseurl/dummyRepo
repo:/some/bogus/local/path
repo:$baseurl/R
SOURCES
darcs get --lazy R S1 && cp fake-sources S1/_darcs/prefs/sources
darcs get --lazy R S2 && cp fake-sources S2/_darcs/prefs/sources

# make sure we do warn about things that are under your control
darcs changes --verbose --repo S1 --no-cache 2>&1 | tee log-local
c1=`grep -c "$baseurl/dummyRepo" log-local`
[ $c1 -eq 1 ]
c2=`grep -c "/some/bogus/local/path" log-local`
[ $c2 -eq 1 ]

# now what about things that aren't?
darcs changes --verbose --repo $baseurl/S2 --no-cache 2>&1 | tee log-remote
c1=`grep -c "$baseurl/dummyRepo" log-remote`
[ $c1 -eq 1 ] # always under your control
not grep -c "/some/bogus/local/path" log-remote
