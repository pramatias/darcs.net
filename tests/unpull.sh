#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
cat > f <<EOF
one
two
three
EOF
darcs rec -Ax -alm init
cp f g
cat > f <<EOF
three
one
EOF
darcs rec -Ax -am foo
echo y | darcs unpull -p foo
cmp f g
cd ..
rm -rf temp1
