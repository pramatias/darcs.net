#!/usr/bin/env bash

set -ev

rm -rf tmp
mkdir tmp
cd tmp
darcs init

empty='test ! -s'
nonempty='test -s'

rm -f foo
darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

>foo
darcs add foo >stdout 2>stderr
$empty stdout
$empty stderr

darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

rm foo
darcs add foo >stdout 2>stderr && exit 1 || true
$empty stdout
$nonempty stderr

cd ..
rm -rf tmp
