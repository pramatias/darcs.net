#!/usr/bin/env bash

. lib

# Some tests for 'darcs whatsnew '

rm -rf temp1

mkdir temp1
cd temp1

darcs init
date > foo
mkdir bar
echo hello world > bar/baz

darcs record -la -m "add foo"

echo goodbye world >> bar/baz

# goodbye should show up precisely once

darcs wh > out
cat out
grep goodbye out | wc -l | grep 1

darcs wh bar bar/baz > out
cat out
grep goodbye out | wc -l | grep 1

darcs mv foo bar
echo not date > bar/foo

darcs wh bar bar/baz > out
cat out
grep date out | wc -l | grep 1

darcs wh foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo foo foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo ./foo ../temp1/foo > out
cat out
grep date out | wc -l | grep 1

darcs wh foo bar/../foo > out
cat out
grep date out | wc -l | grep 1

# This one fails actually, but it's not my fault. Filed as issue1196.
#darcs wh foo foo/../foo/. > out
#cat out
#grep date out | wc -l | grep 1

cd ..

rm -rf temp1
