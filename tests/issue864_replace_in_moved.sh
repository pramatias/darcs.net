#!/usr/bin/env bash

# Issue 864

# darcs mv file1 file2
# edit file2
# darcs replace fails if new token includes some existing edits

# Regression in darcs2 - it works in darcs1 1.0.9

set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
cat <<EOF > file1
aa
bb
cc
aa
bb
cc
EOF
darcs add file1
darcs record -a -m "0" --author X

cat <<EOF > file1
aaaa
bb
cc
aa
bb
cc
EOF

darcs replace aa aaaa file1 > out
cat out
grep 'Skipping file' out && exit 1

darcs mv file1 file2

cat <<EOF > file2
aaaa
beebee
cc
aa
bb
cc
EOF

darcs replace bb beebee file2 > out
cat out
grep 'Skipping file' out && exit 1

cd ..
rm -rf temp
