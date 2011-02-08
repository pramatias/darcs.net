#!/usr/bin/env bash
set -ev

# For issue855: wish: avoid taking lock if using --dry-run
chmod -R u+w temp2 || :
rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
cd ..
mkdir temp2
cd temp2
darcs init
touch x
darcs add x
darcs record -am "test"
cd ..
chmod -R u-w temp2
cd temp2
# need to capture this failure so that we can still
# chmod -R u+w the directory even if we fail
darcsexit=0
darcs push --dry-run ../temp1 || darcsexit=$?
cd ..
chmod -R u+w temp2 # so that other scripts can cleanup
if [ $darcsexit -ne 0 ]; then
  exit $darcsexit
fi
