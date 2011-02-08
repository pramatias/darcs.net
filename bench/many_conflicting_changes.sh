#!/bin/sh

set -ev

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init
echo ALL --ignore-times > _darcs/prefs/defaults
perl -e 'print "foo\nbar\n"x100' > foo
darcs rec -la -A foo -m addfoo
cd ..
cp -RPp temp1 temp2

cd temp1
rm foo
darcs rec -a -A foo -m modfoo
cd ..

cd temp2
perl -e 'print "foo\n"x100' > foo
darcs rec -a -A foo -m otherfoo
darcs pull -va ../temp1

cd ..
rm -rf temp1 temp2
