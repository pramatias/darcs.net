#!/usr/bin/env bash
# Some tests for 'darcs obliterate'

. lib

rm -rf temp1

# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

cd temp1
touch a.txt
darcs add a.txt
darcs record -a -m 'adding a' a.txt
touch b.txt
darcs add b.txt
darcs record -a -m 'adding b' b.txt
# extra confirmation for --all
echo an | darcs obliterate -p add | grep -i "really obliterate"
# --last=1
echo n  | darcs obliterate --last 1 | grep -i adding
# automatically getting dependencies
date >> a.txt
darcs record -a -m 'modifying a' a.txt
echo n | darcs obliterate -p 'adding a' > log
grep -i "modifying a" log
grep -i "No patches selected" log
cd ..
rm -rf temp1
