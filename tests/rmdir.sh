#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2 temp3

# initialise temp1
mkdir temp1
cd temp1
darcs initialize
mkdir foo
echo hello world > foo/bar
darcs add foo foo/bar
darcs record -a -m add
cd ..

# get temp1 into temp2
darcs get temp1 temp2
cd temp2
echo hello world > foo/baz
cd ..

# remove a directory from temp1 and record
cd temp1
rm -rf foo
darcs record -a -m del
cd ..

cd temp2
test -e foo/baz
test -e foo/bar
test -d foo

darcs show files --no-pending --no-dir >> files
grep foo/bar files
darcs show files --no-pending --no-fil >> dirs
grep foo dirs
cd ..

darcs pull -a --repodir=temp2 > pullresult
cat pullresult
grep 'Warning: .ot deleting' pullresult

# get temp1 into temp3
darcs get temp1 temp3
cd temp3
darcs obliterate --last 1 -a
echo hello world > foo/baz
cd ..

# remove a directory from temp1 and record
cd temp1
rm -rf foo
darcs record -a -m del
cd ..

cd temp3
test -e foo/baz
test -e foo/bar
test -d foo

darcs show files --no-pending --no-dir >> files
grep foo/bar files
darcs show files --no-pending --no-fil >> dirs
grep foo dirs
cd ..

darcs pull -q -a --repodir=temp3 > pullresult
cat pullresult
test ! -s pullresult

rm -rf temp1 temp2 temp3
