#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
date > temp.c
darcs add temp.c
darcs record --all -A test --name=hi

mkdir d
darcs add d
darcs mv temp.c d/
darcs record --all -A test --name=mvetc
darcs show contents d/temp.c | cmp d/temp.c -

echo y/d/y | tr / \\012 | darcs unrecord
darcs whatsnew
# darcs show contents d/temp.c | cmp d/temp.c -

darcs record --all -A test --name=again
darcs show contents d/temp.c | cmp d/temp.c -

cd ..
rm -rf temp

