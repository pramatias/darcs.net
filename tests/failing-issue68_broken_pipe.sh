#!/usr/bin/env bash

# For issue68, 'don't report "resource vanished" when stdout pipe is broken.'

set -ev

rm -rf temp1 # Another script may have left a mess.

darcs init --repodir temp1

cd temp1

date > f
darcs add f
darcs rec -am firstp
for (( i=0 ; i < 500; i=i+1 )); do
  echo $i >> f;
  darcs rec -am p$i
done

darcs changes 2> err | head

touch correcterr

diff correcterr err

cd ..
