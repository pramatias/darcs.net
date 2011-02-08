#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
cd ..

# put should set default repo
cd temp1
touch 1.txt
darcs add 1.txt
darcs record -a -m foo 1.txt
darcs put ../temp2
test -e _darcs/prefs/defaultrepo
grep temp2 _darcs/prefs/defaultrepo
cd ..

# put to self
cd temp1
not grep temp1 _darcs/prefs/defaultrepo
DIR=`pwd`
# return special message when you try to put put yourself
not darcs put "$DIR" 2> log
not grep temp1 _darcs/prefs/defaultrepo
grep -i "Can't put.*current repository" log
cd ..

rm -rf temp1 temp2
