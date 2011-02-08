#!/usr/bin/env bash
set -ev

# tests for "darcs dist"

not () { "$@" && exit 1 || :; }

rm -rf temp1
mkdir temp1
cd temp1
darcs init
# needs fixed on FreeBSD
darcs dist -v 2> log
not grep error log
cd ..

rm -rf temp1

