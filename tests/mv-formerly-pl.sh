#!/usr/bin/env bash

# Some tests for 'darcs mv'

. lib

rm -rf temp
mkdir temp
cd temp
darcs init

###

echo adding a directory with more than one .. in it should work.
mkdir foo.d
mkdir foo.d/second
mkdir foo.d/second/third
mkdir foo.d/other


touch ./foo.d/other/date.t
darcs add -r foo.d

cd foo.d/second/third

darcs mv ../../other/date.t ../../other/date_moved.t

cd ../../..
echo darcs refuses to move to an existing file
touch ping
touch pong
darcs add ping pong

not darcs mv ping pong 2>&1 | grep "already exists"

# case sensitivity series
# -----------------------
# these are tests designed to check out darcs behave wrt to renames
# where the case of the file becomes important

# are we on a case sensitive file system?
touch is_it_cs
rm -f IS_IT_CS

if test -e is_it_cs; then
  echo This is a case-sensitive file system.
else
  echo This is NOT a case-sensitive file system.
fi

# if the new file already exists - we don't allow it
# basically the same test as mv ping pong, except we do mv ping PING
# and both ping and PING exist on the filesystem
echo "case sensitivity - simply don't allow mv if new file exists"
touch 'cs-n-1'; touch 'CS-N-1';
touch 'cs-y-1'; touch 'CS-Y-1';
darcs add cs-n-1 cs-y-1

if test -e is_it_cs; then
  # regardless of case-ok, we do NOT want this mv at all
  not darcs mv cs-n-1 CS-Y-1 2>&1 | grep "already exists"

  not darcs mv --case-ok cs-n-1 CS-Y-1 2>&1 | grep "already exists"
fi

# if the new file does not already exist - we allow it
echo "case sensitivity - the new file does *not* exist"
touch 'cs-n-2';
touch 'cs-y-2';
darcs add cs-n-2 cs-y-2
# these mv's should be allowed regardless of flag or filesystem
darcs mv cs-n-2 CS-N-2
darcs mv --case-ok cs-y-2 CS-Y-2

# parasites - do not accidentally overwrite a file just because it has a
# similar name and points to the same inode.  We want to check if a file if the
# same NAME already exists - we shouldn't care about what the actual file is!
echo "case sensitivity - inode check";
touch 'cs-n-3';
touch 'cs-y-3';
darcs add cs-n-3 cs-y-3

if ln cs-n-3 CS-N-3; then # checking if we support hard links
  ln cs-y-3 CS-Y-3
  # regardless of case-ok, we do NOT want this mv at all
  not darcs mv cs-n-3 CS-N-3 2>&1 | grep "already exists"

  not darcs mv --case-ok cs-y-3 CS-Y-3 2>&1 | grep "already exists"
fi

# parasites - we don't allow weird stuff like mv foo bar/foo just because
# we opened up some crazy exception based on foo's name
echo 'refuses to move to an existing file with same name, different path'
touch 'cs-n-4'; touch 'foo.d/cs-n-4';
touch 'cs-y-4'; touch 'foo.d/cs-y-4';
darcs add cs-n-4
# regardless of case-ok, we do NOT want this mv at all
not darcs mv cs-n-4 foo.d/cs-n-4 2>&1 | grep "already exists"

not darcs mv --case-ok cs-y-4 foo.d/cs-y-4 2>&1 | grep "already exists"

# ---------------------------
# end case sensitivity series

touch abs_path.t
darcs add abs_path.t
REPO_ABS=`pwd`
darcs mv "$REPO_ABS/abs_path.t" abs_path_new.t
darcs mv abs_path_new.t "$REPO_ABS/abs_path.t"


# issue608

   touch 'gonna_be_deleted';
   darcs add gonna_be_deleted
   darcs record -am 'added doomed file'
   rm gonna_be_deleted
   darcs record -am 'deleted file'
   touch 'new_file';
   darcs add new_file
   darcs mv new_file gonna_be_deleted


