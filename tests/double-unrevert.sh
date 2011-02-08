#!/usr/bin/env bash
set -ev

# This demonstrates a bug that happens if you revert followed by
# a partial unrevert and a full unrevert.  It requires that
# the second unrevert is working with patches who's contents need
# to be modified by the commute in the first unrevert.

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo line1 >> A
echo line2 >> A
echo line3 >> A
echo line4 >> A
echo line5 >> A
echo line6 >> A
darcs add A
darcs record -am A
sed 's/line2/Line2/' A  > A1; rm A; mv A1 A
sed '4d' A > A1; rm A; mv A1 A
sed 's/line6/Line6/' A > A1; rm A; mv A1 A
darcs revert -a
echo nyn | darcs unrev
darcs unrev -a

cd ..
rm -rf temp1
