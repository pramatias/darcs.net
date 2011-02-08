#!/usr/bin/env bash

# Some tests for 'darcs add'

. lib

rm -rf temp1 temp2

# set up the repository
mkdir temp1
cd temp1
darcs init

# Make sure that messages about directories call them directories
mkdir foo.d
mkdir oof.d
darcs add foo.d
darcs add oof.d
not darcs add foo.d 2>&1 | grep -i directory
# Try adding the same directory when it's already in the repo
not darcs add foo.d oof.d 2>&1 | grep -i directories

# Make sure that messages about files call them files
touch bar
touch baz
darcs add bar
darcs add baz
not darcs add bar 2>&1 | grep -i "following file is"
not darcs add bar baz 2>&1 | grep -i "following files are"

# Make sure that messages about both files and directories say so
not darcs add bar foo.d 2>&1 | grep -i 'files and directories'


# Make sure that parent directories are added for files
mkdir -p a.d/aa.d/aaa.d
mkdir -p b.d/bb.d
touch a.d/aa.d/aaa.d/baz
touch a.d/aa.d/aaa.d/bar
darcs add a.d/aa.d/aaa.d/bar a.d/aa.d/aaa.d/baz b.d/bb.d 2> log
test ! -s log # no output

# Make sure that darcs doesn\'t complains about duplicate adds when adding parent dirs.
mkdir c.d
touch c.d/baz
darcs add c.d/baz c.d 2> log
test ! -s log # no output

# Make sure that add output looks good when adding files in subdir
mkdir d.d
touch d.d/foo
darcs add -rv d.d | grep 'd.d/foo'

# 'adding a non-existent dir and file gives the expected message
not darcs add notadir/notafile 2>&1 | grep -i 'does not exist'

cd ..
rm -rf temp1
