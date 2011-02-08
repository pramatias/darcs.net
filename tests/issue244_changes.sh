#!/usr/bin/env bash
set -ev

# Issue244
# darcs changes should be able to pull up the history for a file
#   using its moved and not-yet recorded new name

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch b
darcs add b
darcs record -am 11
darcs mv b c
darcs changes c | grep 11
cd ..
rm -rf temp1
