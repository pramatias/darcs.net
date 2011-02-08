#!/usr/bin/env bash

# http://bugs.darcs.net/issue803: Darcs 2.0 regression on manual renames


. lib

rm -rf temp
mkdir temp
cd temp
darcs init

touch a.txt
darcs add a.txt
darcs record -a -m "First" -A me

mkdir subdir
darcs add subdir
darcs record -a -m "Second" -A me

mv a.txt subdir/
darcs mv a.txt subdir/a.txt
darcs record -a -m "Third" -A me

darcs changes --last 1 -v > stdout
cat stdout

not grep 'rmfile' stdout

cd ..
rm -rf temp
