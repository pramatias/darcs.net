#!/usr/bin/env bash
set -ev

uname | grep "MINGW" > /dev/null && exit 0

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo record author me > _darcs/prefs/defaults
echo ALL all >> _darcs/prefs/defaults
echo ALL verbose >> _darcs/prefs/defaults
echo ALL ignore-times >> _darcs/prefs/defaults
touch foo
darcs add foo
darcs record -m add_foo
echo hello >> foo
darcs record -m mod_foo
cd ..
darcs get --repo-name temp2 temp1
cd temp2
cp ../temp1/_darcs/prefs/defaults _darcs/prefs
echo y/d/y | tr / \\012 | darcs unpull --interactive
test -f foo -a ! -s foo
chmod +x foo
test -x foo
darcs pull ../temp1
test -x foo
cd ..
rm -rf temp1 temp2

