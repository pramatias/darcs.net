#!/usr/bin/env bash
set -ev

DARCS_EDITOR=echo
export DARCS_EDITOR
export SENDMAIL=`which true`

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init

# setup test
cd ../temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x

# Test that --cc is also printed as recipient in case of success
darcs send --author=me -a --to=random@random --cc=foo@example.com ../temp2 2>&1|grep -i foo@example.com

# Test that --cc is also printed as recipient in case of error
darcs send --author=me -a --to=random@random --cc=foo@example.com ../temp2 2>&1|grep -i foo@example.com

cd ..
rm -rf temp1 temp2
