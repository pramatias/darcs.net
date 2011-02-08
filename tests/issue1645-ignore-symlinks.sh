#!/usr/bin/env bash
## Test for issue1645 - Since Darcs does not version-contol symlinks, 
## it should not follow them, ESPECIALLY symlinks to directories
## outside the repository. All these tests are passed with darcs-2.2
##
## See path_resolution(7) and symlink(7) for more info, especially
## the former.
##
## There's a second section to this test for systems that support
## case-folding.  See issue1645-ignore-symlinks-case-fold.sh
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

darcs --version

add_to_boring() {
  echo "$1" >> _darcs/prefs/boring
}

## These are the simple does-the-wrong-thing errors.
cd R
touch log
add_to_boring '^log$'

unset pwd # Since this test is pretty much linux-specific, hspwd.hs is not needed
abort_windows # and skip if we are on win32...

# Case 1: looping symlink to non-recorded non-boring dir
mkdir non-recorded-dir
ln -s ../non-recorded-dir ./non-recorded-dir/loop               # relative symlink
ln -s "`pwd`"/non-recorded-dir ./non-recorded-dir/loop2           # absolute symlink
darcs w -l >log 2>&1                                            # should not loop
darcs rec -alm "added ./non-recorded-dir" >>log 2>&1            # should not loop
darcs changes -s --patch="added ./non-recorded-dir" >>log 2>&1  # should report only dir, not symlink
not grep -vE "(^ *$|^\+|[0-9]:[0-9][0-9]:[0-9]|./non-recorded-dir)" log

# Case 2: looping symlink to recorded dir
mkdir recorded-dir
darcs add recorded-dir
darcs rec -am "added recorded-dir"
ln -s ../recorded-dir ./recorded-dir/loop      # relative symlink
ln -s "`pwd`"/recorded-dir ./recorded-dir/loop2  # absolute symlink
not darcs w -l >log 2>&1                       # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1  # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log

# Case 3: looping symlink to boring dir
mkdir boring-dir
add_to_boring '^boring-dir$'
ln -s ../boring-dir ./boring-dir/loop
ln -s "`pwd`"/boting-dir ./boring-dir/loop2
not darcs w -l >log 2>&1                      # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1 # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log

# Case 4: non-looping symlink to non-recorded non-boring dir
mkdir non-recorded-dir2
ln -s ./non-recorded-dir2 link
ln -s "`pwd`"/non-recorded-dir2 ./link2
darcs w -l >log 2>&1                                             # should report only "non-recorded-dir2"
darcs rec -alm "added ./non-recorded-dir2" >>log 2>&1            # should add only dir, not symlink
darcs changes -s --patch="added ./non-recorded-dir2" >>log 2>&1  # should report only dir, not symlink
not grep -vE "(^ *$|^\+|[0-9]:[0-9][0-9]:[0-9]|./non-recorded-dir2)" log
rm link link2

# Case 5: non-looping symlink to recorded dir
ln -s ./recorded-dir ./link
ln -s "`pwd`"/recorded-dir ./link2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 6: non-looping symlink to boring dir
ln -s ./boring-dir ./link
ln -s "`pwd`"/boring-dir ./link2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 7: symlink pointing outside the repo
ln -s ../S link
(cd ..; ln -s "`pwd`"/S ./R/link2)
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 8: symlink to non-recorded non-boring file
touch non-recorded-file
ln -s ./non-recorded-file ./link
ln -s "`pwd`"/non-recorded-file ./link2
darcs w -l >log 2>&1                                             # should report only "non-recorded-file"
darcs rec -alm "added ./non-recorded-file" >>log 2>&1            # should add only file, not symlink
darcs changes -s --patch="added ./non-recorded-file" >>log 2>&1  # should report only file, not symlink
not grep -vE "(^ *$|^\+|[0-9]:[0-9][0-9]:[0-9]|./non-recorded-file)" log
rm link link2

# Case 9: symlink to recorded file
echo "some content" > recorded-file
darcs add recorded-file
darcs rec -am "added recorded-file" recorded-file
ln -s ./recorded-file ./link
ln -s "`pwd`"/recorded-file ./link2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 10: symlink to boring file
ln -s ./log ./link
ln -s "`pwd`"/log ./link2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 11: dangling symlink
ln -s /completely/bogus/path ./link
ln -s ../../../../not/exist ./link2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm link link2

# Case 12: self-referencing link
ln -s l l
ln -s "`pwd`"/l2 ./l2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm l l2

# Case 13: link to device file outside the repo
ln -s /dev/zero l
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm l

# Case 14: link to fifo
mkfifo f
ln -s f l
ln -s "`pwd`"/f ./l2
not darcs w -l >log 2>&1                                         # expecting "No changes!"
darcs rec -alm "should not happen" >>log 2>&1                    # expecting "No changes!" as well
not grep -vE "(^ *$|^\+|No changes!)" log
rm f l l2
