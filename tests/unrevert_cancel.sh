#!/usr/bin/env bash

# From issue366 bug report

set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch a 
touch b
darcs add *
darcs record -A moi -am init
echo plim >> a
echo plim >> b
echo y/y/y/ | tr / \\012 | darcs revert
echo ploum >> a 
echo nyy | tr / \\012 | darcs unrevert

cd ..
rm -rf temp1
