#!/usr/bin/env bash
set -ev

rm -rf temp-$$
mkdir temp-$$
cd temp-$$
set -e
darcs initialize
echo text > afile.txt
darcs add afile.txt
darcs record --author me --all --no-test --name init
darcs diff
darcs diff -p . --store-in-mem > diffinmem
darcs diff -p . --no-store-in-mem > diffondisk
diff diffinmem diffondisk
cd ..

rm -rf temp-$$
