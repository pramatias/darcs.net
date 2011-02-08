#!/usr/bin/env bash
## Test for issue1645 - Since Darcs does not version-contol symlinks,
## it should not follow them, ESPECIALLY symlinks to directories
## outside the repository. All these tests are passed with darcs-2.2
##
## See path_resolution(7) and symlink(7) for more info, especially
## the former.
##
## This only covers the case-folding test cases.
## See also the issue1645-ignore-symlinks for the main test
##
## Copyright (C) 2010  Trent W. Buck, Dmitry Astapov
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

add_to_boring() {
  echo "$1" >> _darcs/prefs/boring
}

## These are the simple does-the-wrong-thing errors.
cd R
touch log
add_to_boring '^log$'

unset pwd # Since this test is pretty much linux-specific, hspwd.hs is not needed

# Skip the case-folding tests on systems that don't support it
touch cs-test
ln -s cs-test cs-Test || exit 200

rm cs-test cs-Test # move file and symlink out of the way for real tests

# Case 15: case-folding link to non-recorded file
touch non-recorded-file2
ln -s ./non-recorded-file2 ./Non-Recorded-File2
ln -s "`pwd`"/non-recorded-file2 ./Non-ReCoRdEd-File2
darcs w -l >log 2>&1                                             # should report only "non-recorded-file"
darcs rec -alm "added ./non-recorded-file2" >>log 2>&1            # should add only file, not symlink
darcs changes -s --patch="added ./non-recorded-file2" >>log 2>&1  # should report only file, not symlink
not grep -vE "(^ *$|^\+|[0-9]:[0-9][0-9]:[0-9]|./non-recorded-file2)" log
rm Non-Recorded-File2 ./Non-ReCoRdEd-File2

# Case 16: case-folding link to recorded file
ln -s ./recorded-file ./Recorded-File
ln -s "`pwd`"/recorded-file ./ReCorded-File
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm Recorded-File ReCorded-File
