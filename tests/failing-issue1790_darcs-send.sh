#!/usr/bin/env bash
## Test for issue1790 - darcs send --context foo should not require
## a remote repository
##
## Copyright (C) 2009 Loup Vaillant
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
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
   # populate R (optional)
   echo foo > foo.txt
   darcs add foo.txt
   darcs record -a -m "foo"

   # create a context file to represent R's state
   darcs changes --context > ../R.context
cd ..

# copy the repository the hard way.
# normally one would use `darcs get A B` to do that,
# but when no central server is available, this sort of
# hard copy may be perceived as simpler: Just tar xcfz
# the repository and send it via email.
cp -r R S

cd S
   # Make sure there is no _darcs/prefs/defaultrepo file
   # If there is one even on new repositories, that would
   # appear to solve the issue, but I think this is not a
   # good solution: the absence of the defaultrepo file
   # is useful to indicate the absence of interaction with
   # remote repositories.
   #
   # Of course, at the time of this writing, the following
   # line has no effect
   rm -rf _darcs/prefs/defaultrepo

   # make some further modifications (optional)
   echo bar > bar.txt
   darcs add bar.txt
   darcs record -a -m "bar"

   # try to send a patch to the first repository,
   # using its context file
   darcs send -a -o ../bar.patch --context=../R.context
cd ..
