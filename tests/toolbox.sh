#!/usr/bin/env bash

. lib

DIR="`pwd`"

abort_windows

# set up the repository
rm -rf temp1                    # another script may have left a mess.
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
# trick: hitting 'enter' in interactive darcs record
echo n/ | tr / \\012 | darcs record
cd ..
rm -rf temp1
