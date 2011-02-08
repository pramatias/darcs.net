#!/usr/bin/env bash
set -ev
export DARCS_EMAIL=test
rm -rf tmp_d1 tmp_d2 tmp_d
# Preparations:
# Set up two repos, each with a patch (d1 and d2 respectively) with both
# an individual change and a change that is identical in both repos,
# and thus auto-merge, i.e., they don't conflict in darcs2.  Pull them
# together and record a patch (problem) on top of the auto-merged change,
# so that it depends on EITHER of two patches.

mkdir tmp_d1; cd tmp_d1
darcs init --darcs-2
echo a > a
echo b > b
echo c > c
darcs rec -alm init
echo a-independent > a
echo c-common > c
darcs rec -am d1
cd ..
darcs get --to-patch init tmp_d1 tmp_d2
cd tmp_d2
echo b-independent > b
echo c-common > c
darcs rec -am d2
darcs pull -a ../tmp_d1
# no conflicts -- c-common is identical
echo c-problem > c
darcs rec -am problem
cd ..

# I want to pull the 'problem' patch, but expect darcs to get confused
# because it doesn't know how to select one of the two dependent patches.

darcs get --to-patch init tmp_d2 tmp_d
cd tmp_d
echo n/n/y |tr / \\012 |darcs pull ../tmp_d2
darcs cha

# This is weird, we got d2 though we said No.  I would have expected
# darcs to skip the 'problem' patch in this case.

# Try to pull d1 and unpull d2.

darcs pull -a ../tmp_d1
exit 1 # darcs hangs here (2.0.2+77)!
echo n/y/d |tr / \\012 |darcs obl -p d2

# The obliterate fails with: patches to commute_to_end does not commutex (1)

cd ..
rm -rf tmp_d1 tmp_d2 tmp_d
