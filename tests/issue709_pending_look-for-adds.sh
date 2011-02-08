#!/bin/sh
set -ve

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

# Here we check whether recording just one of two --look-for-add
# addfiles causes any trouble (which it doesn't)

date > f1
date > f2
echo yyd | darcs record -l -m ff

cat _darcs/patches/pending

not darcs wh
rm f2

# Try recording a file add without --look-for-adds, with a setpref
# patch present that we don't record.

darcs setpref boringfile .boring

echo bar > bar
darcs add bar
echo yyd | darcs record -mbar

cat _darcs/patches/pending

darcs whatsnew -s

test -z "`darcs whatsnew -s`"

# Now try the same thing using --look-for-adds

echo foo > foo

darcs wh -l

# remove any files added by profiling or hpc...
rm -f darcs.tix darcs.prof

echo yyd | darcs record --look-for-adds -mfoo

cat _darcs/patches/pending

darcs whatsnew -s

test -z "`darcs whatsnew -s`"

cd ..
rm -rf temp1
