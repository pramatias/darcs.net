#!/usr/bin/env bash

# Issue595
#
# A test for running "darcs get" when the parent directory has restrictive
# permissions.  The bug is that darcs trys to "chdir" to the current directory
# using the full path.  The permissions on the parent directory prevent this
# from working, even though the current repo and the remote have sufficient
# permissions.
#
# The real-world case where this would happen would be a web-server with
# restrictive permissions on "/home", with a user running darcs within that.

. lib

abort_windows

rm -rf temp1 temp2

# Set up a "remote" repo
mkdir tmp_remote
cd tmp_remote
darcs 'init'
cd ..

DIR=`pwd`
# Set up a directory with restrictive permissions
mkdir -p tmp_restrictive/liberal
cd tmp_restrictive/liberal
chmod 0111 ../../tmp_restrictive
# sanity check that we can cd out and back
cd ../..; cd tmp_restrictive/liberal
# TODO: we avoid this test on Solaris because it seems we can't create
# anything in tmp_restrictive/liberal
touch can_touch
if [ -e can_touch ]; then
  if hwpwd; then
    darcs get "$DIR/tmp_remote" 2> log
    not grep -i 'permission denied' log
  else
    echo "Apparently I can't do `basename $0` on this platform"
  fi
else
  echo "Can't do `basename $0` on this platform"
fi
cd "$DIR"
# We have to fix the permissions, just so we can delete it.
chmod 0755 tmp_restrictive

rm -rf tmp_remote tmp_restrictive
