#!/usr/bin/env bash

# Some tests for launching external commands

. lib

rm -rf temp1

touch_fakessh='./touch-fakessh'
if echo $OS | grep -i windows; then
  touch_fakessh="touch_fakessh.bat"
fi
export DARCS_SSH=$touch_fakessh
export DARCS_SCP=$touch_fakessh
export DARCS_SFTP=$touch_fakessh
rm -rf 'fakessh'
rm -rf 'touch-fakessh'

# make our ssh command one word only
echo 'echo hello > fakessh' > $touch_fakessh
chmod u+x $touch_fakessh
# first test the DARCS_SSH environment variable
not darcs get example.com:foo
grep hello fakessh
rm -f fakessh

# now make sure that we don't launch ssh for nothing
mkdir temp1
cd temp1
darcs init
cd ..
darcs get temp1 > log
not grep fakessh log
not darcs get http://darcs.net/nonexistent
not grep fakessh log
cd ..
rm -rf temp1
