#!/usr/bin/env bash
set -ev

# This checks for a possible bug in patch selection where the no available
# patches case is hit.

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch A
darcs add A
darcs record -am A
echo 'l1' >> A
darcs record -am l1
darcs amend-record -a --patch 'A'

cd ..
rm -rf temp1
