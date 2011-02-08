#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs initialize
echo ALL ignore-times >> _darcs/prefs/defaults
echo A > foo
darcs add foo
darcs record -a -m AA -A x
echo B > foo
darcs record -a -m BB -A x
echo C > foo
darcs record -a -m CC -A x
darcs tag -m 1.0 -A x
echo D > foo
darcs record -a -m DD -A x
echo E > foo
darcs record -a -m EE -A x
echo F > foo
darcs record -a -m FF -A x
darcs tag -m 2.0 -A x

darcs show tags > my
cat my
cat > ref <<EOF
2.0
1.0
EOF

cmp my ref
cd ..

rm -rf temp1
