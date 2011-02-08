#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo bar
darcs add foo bar

for (( i=0 ; i < 5; i=i+1 )); do
  echo $i >> file-$i;
  darcs add file-$i
done

cd ..

rm -rf temp1

