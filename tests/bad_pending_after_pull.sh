#!/usr/bin/env bash

set -ev

rm -fr temp1 temp2

mkdir temp1
cd temp1
darcs init

echo abc > A
darcs add A
echo def > B1
darcs add B1
# darcs record -all --name patch1 # this way it doesn't trigger the bug
for i in 1 2 3 4 5 6 7 8 9 11; do echo y; done | darcs record --name patch1

darcs mv B1 B2
darcs record --all --name patch2
cd ..

mkdir temp2
cd temp2
darcs init
darcs pull --all ../temp1
darcs whatsnew | grep 'No changes'
cd ..

rm -fr temp1 temp2

# issue494: note that in this test, we deliberately select filenames
# with a backwards sorting order
mkdir temp1
cd temp1
darcs init
echo abc > b
darcs add b
darcs record --all -m patch1
darcs mv b a
echo def > a
darcs record --all -m patch2
cd ..

mkdir temp2
cd temp2
darcs init
darcs pull --all ../temp1
darcs whatsnew | grep 'No changes'
cd ..

rm -fr temp1 temp2
