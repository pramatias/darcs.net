#!/usr/bin/env bash
set -ev

not () { "$@" && exit 1 || :; }

rm -rf temp1
mkdir temp1
cd temp1

darcs init
touch aargh
darcs add aargh
echo utrecht > aargh

darcs wh foo foo/../foo/. > out
cat out
not grep utrecht out

cd ..
rm -rf temp1

