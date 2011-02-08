#!/usr/bin/env bash
set -ev

rm -rf temp temp_0
mkdir temp
cd temp
darcs init
date > bla
darcs add bla
darcs record -a --name=11
echo hello > world
darcs add world
darcs whatsnew --dont-look-for-adds > wn1
cd ..
darcs get temp
cd temp_0
date > bla2
date >> bla
darcs add bla2
darcs record -a --name=22
darcs push -a ../temp
cd ..
cd temp
darcs whatsnew --dont-look-for-adds > wn2
diff wn1 wn2

darcs record -a -m 'cleaning up for new test.'
date > foo.jpg
darcs add foo.jpg
darcs whatsnew

darcs remove foo.jpg
darcs whatsnew && exit 1

cd ..

rm -rf temp temp0
