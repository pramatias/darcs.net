#!/usr/bin/env bash

# For issue390: darcs whatsnew somefile" lstats every file in the working copy and pristine/ directory

set -ev

if ! test -x "$(which strace)"
then echo skipping test since strace was not found
     exit
fi

rm -rf temp
mkdir temp
cd temp
darcs init
date > file1
date > file2
darcs add file*
darcs record -am "test"

strace darcs whatsnew file1 &> out
# we should be accessing file1
grep file1 out
# but shouldn't be accessing file2
if grep file2 out
then
    echo A whatsnew for file1 should not involve a 'stat' call to file2
    exit 1
else
    echo Yay.  We pass.
fi

rm -rf temp
