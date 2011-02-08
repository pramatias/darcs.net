#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs initialize
echo text > afile.txt
darcs add afile.txt
darcs record --author me --all --no-test --name init
mkdir d
echo The following mv should fail, since d isnt in the repo.
if darcs mv afile.txt d/afile.txt; then
false
fi

# Now clean up.
cd ..
rm -rf temp

