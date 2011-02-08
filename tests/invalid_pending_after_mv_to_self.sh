#!/usr/bin/env bash

# A regression test for issue567

set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
mkdir dir
touch dir/t.t
darcs add dir
darcs add dir/t.t
darcs record -am 'initial add'

# grand finale? Can we move the file to itself? 
darcs mv dir/t.t dir/

cd ..

rm -rf temp
