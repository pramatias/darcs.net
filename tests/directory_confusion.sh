#!/usr/bin/env bash
set -ev

T=temp
rm -rf "$T"
mkdir "$T"
echo "$T"
cd "$T"
set -e
darcs initialize
echo text > afile.txt
darcs add afile.txt
darcs record --author me --all --no-test --name init
mkdir d
darcs add d
mkdir d/e
darcs add d/e
darcs mv afile.txt d/e/afile.txt
echo altered_text > d/e/afile.txt
darcs record --author me --all --no-test --name confusion
test ! -f _darcs/pristine/afile.txt
echo y/d/y | tr / \\012 | darcs unrecord
rm -rf "$T"
