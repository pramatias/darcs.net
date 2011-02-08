#!/usr/bin/env bash
set -ev

rm -rf fooOld tempA tempB
mkdir fooOld tempA tempB
cd fooOld
darcs init
echo record author me > _darcs/prefs/defaults
echo ALL all >> _darcs/prefs/defaults
#echo ALL verbose >> _darcs/prefs/defaults
echo ALL ignore-times >> _darcs/prefs/defaults
echo Old > foo
darcs add foo
darcs record -m Old
cd ..

cd tempA
darcs init
cp ../fooOld/_darcs/prefs/defaults _darcs/prefs
darcs pull ../fooOld
echo A > foo
darcs record -m AA
cd ..

cd tempB
darcs init
cp ../fooOld/_darcs/prefs/defaults _darcs/prefs
darcs pull ../fooOld
echo B > foo
darcs record -m BB
darcs pull ../tempA
echo A > foo
darcs record -m "ok A's fine."
cd ..

# At this point, tempB and tempA should agree--since the conflict was
# resolved in favor of tempA.
cmp tempB/foo tempA/foo

cd tempA
echo AA > foo
darcs record -m "AA -- upping the ante."
cd ..

cd tempB
darcs pull ../tempA
cd ..

cd tempA
darcs pull ../tempB
cd ..

# At this point, tempB and tempA should agree since we have pulled both ways.
cmp tempB/foo tempA/foo

