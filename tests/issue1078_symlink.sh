#!/usr/bin/env bash

. lib

if echo $OS | grep -i windows; then
    echo this test does not work on windows because
    echo windows does not have symlinks
    exit 0
fi

rm -rf temp1 temp2
mkdir temp1
ln -s temp1 temp2
cd temp2
darcs init
touch a b
DIR=`pwd`
darcs add "${DIR}/../temp1/a" # should work, just to contrast with the case below
darcs add "${DIR}/b"          # this is the case we are testing for
cd ..
rm -rf temp1 temp2
