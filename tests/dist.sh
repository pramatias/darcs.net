#!/usr/bin/env bash

# run darcs dist, then extract the resulting archive
# and compare it to the original repository content

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

for (( i=0 ; i < 5; i=i+1 )); do
  echo $i >> file-$i;
  mkdir dir-$i;
  echo $i >> dir-$i/file-$i;
  darcs add file-$i;
  darcs add dir-$i/file-$i
done

darcs record -a -m add_foo | grep -i "finished recording"
darcs dist

mv temp1.tar.gz  ..

cd ..

rm -rf temp1/_darcs
mv temp1 temp_orig

tar xzf temp1.tar.gz

diff -r temp_orig temp1

rm -rf temp_orig temp1
