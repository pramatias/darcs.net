#!/bin/sh

set -ev

## I would use the builtin !, but that has the wrong semantics.
not () { "$@" && exit 1 || :; }

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo first > a
darcs add a
darcs record -am 'first'
echo replace first with something else > a
darcs record -am 'mistake'
echo first > a
echo on second thought >> a
echo ya | darcs amend-record -a
darcs changes --last=1 -v > output
cat output
not grep first output
cd ..
rm -rf temp1
