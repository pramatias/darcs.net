#!/bin/sh

set -ev

not () { "$@" && exit 1 || :; }

rm -rf temp
rm -rf dist1269.tar.gz
mkdir temp
cd temp

darcs init
printf "Line1\nLine2\nLine3\n" > foo
darcs record -alm Base

darcs setpref predist false

# It is a bug in darcs if the dist succeeds. It should
# fail with a non-zero exit code
not darcs dist -d dist1269

# It is a bug in darcs if the dist file has been created
# it should /not/ be created if the predist cmd returned
# a non-zero exit code
not test -f dist1269.tar.gz

cd ..
rm -rf temp
