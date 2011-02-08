#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2 temp3
mkdir temp2
cd temp2
darcs init
cd ..

mkdir temp1
cd temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m foobar -A author
darcs send -a --dont-edit-description -o ../temp3 ../temp2
test -f ../temp3

darcs send -a --dont-edit-description -o ../temp2/patchfile ../temp2
test -f ../temp2/patchfile

mkdir subdir
darcs send -a --dont-edit-description -o subdir/../../temp2/patchfile1 ../temp2
test -f ../temp2/patchfile1

cp ../temp3 correct

cd subdir
darcs send -a --dont-edit-description -o ../patchfile ../../temp2
diff -u ../patchfile ../correct

rm ../patchfile
darcs send -a --set-default --dont-edit-description -o - ../../temp2 > ../patchfile
grep -v Creating ../patchfile | diff -u ../correct -

darcs apply --repodir=../../temp2 --dry-run ../patchfile > out
cat out
grep foobar out

cd ../..

cd temp2

darcs apply --dry-run ../temp3 > out
cat out
grep foobar out

darcs apply --dry-run ../temp1/correct > out
cat out
grep foobar out

darcs apply --dry-run ../temp1/patchfile > out
cat out
grep foobar out

darcs apply --dry-run ../temp3 > out
cat out
grep foobar out

darcs apply --dry-run ../temp3 > out
cat out
grep foobar out

cd ..

rm -rf temp1 temp2 temp3

