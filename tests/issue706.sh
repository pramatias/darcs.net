#!/usr/bin/env bash
set -ev

# for issue706: "Filenames with spaces issue"

DARCS_EDITOR=echo
export DARCS_EDITOR

rm -rf temp
mkdir temp
cd temp
darcs init

touch 'A B'

darcs add 'A B'
darcs rec -a -m 'a b' -A me
ls
darcs check

cd ..
rm -rf temp
