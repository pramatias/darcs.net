#!/usr/bin/env bash

# This test script, originally written by David Roundy is in the public
# domain.

set -ev

rm -rf temp1 temp2

mkdir temp1
cd temp1
echo foo > bar
darcs initialize
echo record author me > _darcs/prefs/defaults
darcs add bar
darcs record -a -m addbar

cd ..
darcs get temp1 temp2
cd temp1
date > bar
darcs record -a -m datebar

cd ../temp1
echo aack >> bar
darcs record -a -m aackbar

cd ../temp2

darcs pull -av
darcs check

cd ..
rm -rf temp1 temp2
