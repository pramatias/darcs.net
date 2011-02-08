#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
echo ALL ignore-times > _darcs/prefs/defaults
echo a > foo
darcs add foo
darcs record -a -m aa -A test
echo b > foo
echo y | darcs revert -a
echo y/d/y | tr / \\012 | darcs unrecord
# since the unrevert is impossible, we should fail if it succeeds...
echo y | darcs unrevert && exit 1 || true

# now let's try a possible unrevert, just for fun...
echo b >> foo
darcs record -a -m bb -A test
echo f/b | tr / \\012 > foo
darcs record -a -m 'aaa becomes f' -A test
date >> foo
echo y | darcs revert -a
echo y/d/y | tr / \\012 | darcs unpull
# Now add the date back on at the end:
echo y | darcs unrevert
echo 'M ./foo +1' > correct_summary
darcs whatsnew --dont-look-for-adds --summary > actual_summary
diff -c correct_summary actual_summary

cd ..
rm -rf temp
