#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs init

echo stuff > foo
darcs add foo

darcs revert -a
