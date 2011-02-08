#!/usr/bin/env bash
set -ev

. lib
rm -rf temp
mkdir temp
cd temp
darcs init
mkdir a b
touch a/a b/b
darcs add --rec .
darcs record -a -m ab -A test
darcs annotate a/a
# annotate --repodir=something '.' should work
cd ..
darcs annotate --repodir temp '.'
cd temp

cd ..
rm -rf temp

