#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2

# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

cd temp1
echo -n "from temp1" > one.txt
darcs add one.txt
darcs record -A bar -am "add one.txt"
echo >> one.txt
cd ..

darcs get temp1 temp2
cd temp2
echo "in tmp2" >> one.txt
darcs record -A bar -am "add extra line"
lines_added=`darcs changes -v --last=1 | grep '\+' | wc -l`
echo $lines_added
test $lines_added -eq 1
cd ..

rm -rf temp1 temp2
