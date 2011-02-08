#!/usr/bin/env bash

. lib

# test for working dir woes

# the setup...
rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
mkdir a
echo temp0 > a/x
darcs add a
darcs add a/x
darcs record -am "aa"
darcs mv a/x a/y
darcs record -am "x to y"
echo temp1 > b
darcs add b
darcs record -am "bb"
mkdir d
darcs add d
darcs record -am "dd"
darcs tag 1
echo 1-b2 > b
darcs record -am "b2"
cd ..

# try to move a file that we don't have the right to do
darcs get temp1 temp2 --to-patch aa
cd temp2
chmod u-w a
darcs pull -a
test -e b
chmod u+w a
cd ..
rm -rf temp2

# [issue319] try to overwrite file(s) in our working dir
darcs get temp1 temp2 --to-patch aa
cd temp2
echo temp2 > b
echo temp2 > d
darcs pull -a -t 1
grep temp1 b
grep temp2 b-darcs-backup0
grep temp2 d-darcs-backup0
# now make sure we didn't overdo it
darcs pull -a
grep '1-b2' b
test -e b-darcs-backup0
test ! -e b-darcs-backup1
cd ..
rm -rf temp2

# [issue298] backup working dir files with conflicts
darcs get temp1 temp2 --tag 1
cd temp2
echo 2-b2 > b
darcs pull -a
grep "v v v" b
grep "2-b2"  b-darcs-backup0
not grep "v v v" b-darcs-backup0
cd ..
rm -rf temp2

# [issue440] a) try to overwrite a file in our working dir
darcs get temp1 temp2 --to-patch a
cd temp2
echo temp2 > a/y
echo old-bak > a/y-darcs-backup0
darcs pull -a
grep temp0 a/y
grep old-bak a/y-darcs-backup0
grep temp2   a/y-darcs-backup1
cd ..
rm -rf temp2

# [issue440] b) try to overwrite a directory in our working dir
darcs get temp1 temp2 --to-patch a
cd temp2
mkdir a/y
echo old-bak > a/y-darcs-backup0
darcs pull -a
grep temp0 a/y
grep old-bak a/y-darcs-backup0
test -d a/y-darcs-backup1
cd ..
rm -rf temp2

rm -rf temp1
