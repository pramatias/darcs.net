#!/usr/bin/env bash
## Test for issue1232 - When converting a repo to darcs2 format,
## the prefs file isn't copied.
##
## Copyright (C) 2010  Dino Morelli
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


# Test set-up where src repo DOES NOT already have a
# _darcs/prefs/prefs file

# Another script may have left a mess
rm -rf R S

# Create source repo
darcs init --old-fashioned-inventory --repo R

# Perform the d1 to d2 conversion
echo "I understand the consequences of my action" | darcs convert R S

# Check that the new repo is d2
test -f S/_darcs/hashed_inventory


# Test set-up where src repo DOES have a _darcs/prefs/prefs file

# Another script may have left a mess
rm -rf R S

# Create source repo
darcs init --old-fashioned-inventory --repo R

# Do something easy that will create a prefs file
darcs setpref test 'true' --repo R

# Perform the d1 to d2 conversion
echo "I understand the consequences of my action" | darcs convert R S

# Check that the prefs file was copied over
prefsPath="_darcs/prefs/prefs"
diff R/$prefsPath S/$prefsPath
