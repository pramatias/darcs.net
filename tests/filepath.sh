#!/usr/bin/env bash

# Some tests for proper handling of filepaths

. lib

DIR=`pwd`

rm -rf temp1 temp2

# Make sure that init works with --repodir
darcs init --repodir=temp1
test -d temp1/_darcs

# add some meat to that repository
cd temp1
touch baz
darcs add baz
darcs record -m moo -a
cd ..

# ----------------------------------------------------------------------
# local vs remote filepaths
# ----------------------------------------------------------------------

# trick: OS-detection (if needed)
if echo $OS | grep -i windows; then
  echo This test does not work on Windows
else
  darcs get temp1 temp2
  cd temp2
  mkdir -p dir
  darcs add dir
  cd dir
  touch foo:bar
  darcs add --reserved-ok foo:bar
  cd ../..
  rm -rf temp2
fi

# ----------------------------------------------------------------------
# repodir stuff
# ----------------------------------------------------------------------
mkdir -p temp1/non-darcs
# FIXME: This test does not seem to make much sense
# --repodir is not recursive
not darcs get temp1/non-darcs 2> log
grep "Not a repository" log
rm -rf temp1/non-darcs
rm -rf non-darcs

# get accepts --repodir.
darcs get --repodir=temp2 temp1 | grep -i "Finished getting"
test -d temp2/_darcs
rm -rf temp2
# get accepts absolute --repodir.
darcs get --repodir="${DIR}/temp2" temp1 | grep -i "Finished getting"
test -d temp2/_darcs

# changes accepts --repodir.
darcs changes --repodir=temp1 | grep -i "moo"
# changes accepts absolute --repo.
darcs changes --repo="${DIR}/temp1" | grep -i "moo"
# changes accepts relative --repo.
darcs changes --repo=temp1 | grep -i "moo"
# [issue467] context --repodir
darcs changes --context --repodir=temp1 | grep 'Context:'

# dist accepts --repodir.
darcs dist --repodir=temp1 | grep -i "Created dist"

# optimize accepts --repodir.
darcs optimize --reorder-patches --repodir=temp1 | grep -i "done optimizing"

# repair accepts --repodir.
darcs repair --repodir=temp1 | grep -i "already consistent"

# replace accepts --repodir.
darcs replace --repodir=temp1 foo bar

# setpref accepts --repodir.
darcs setpref --repodir=temp1 test echo | grep -i "Changing value of test"

# trackdown accepts --repodir.
darcs trackdown --repodir=temp1 | grep -i "Success!"

# ----------------------------------------------------------------------
# converting between absolute and relative paths
# ----------------------------------------------------------------------

rm -rf temp3
darcs get temp1 temp3
cd temp3
mkdir -p a/b
cd ..

cd temp2
echo hello 1 >> baz
darcs record -m hello1 -a
echo hello 2 >> baz
darcs record -m hello2 -a
cd ..

# can handle .. path
cd temp3
darcs pull ../temp2 --set-default -p1 --all | grep -i 'Finished pulling'
darcs pull --dry-run | grep hello2
cd a/b
#[issue268] repodir with subdir
darcs pull --dry-run | grep hello2
cd ..
cd ..

rm -rf log temp1 temp2 temp3
