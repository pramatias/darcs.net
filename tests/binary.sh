#!/usr/bin/env bash
set -ve
binary=example_binary.png
function checkbinary(){
  cmp $binary ../temp1/$binary
}
rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
cp $TESTDATA/$binary .
darcs add $binary
darcs record -am P1
cd ../temp2
darcs init
test ! -e $binary
darcs pull ../temp1 -a
checkbinary
darcs obliterate -a
test ! -e $binary
darcs pull ../temp1 -a
checkbinary
darcs unrecord -a
checkbinary
darcs revert -a
test ! -e $binary
darcs unrevert -a
checkbinary
rm -rf temp1 temp2
