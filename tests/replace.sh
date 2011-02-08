#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs init

echo "X X X" > foo
echo $'A A,A\tA,A\vA' >> foo
darcs rec -alm "Added"

# These should fail until replace handles tokens and
# token-chars with leteral spaces in them
darcs replace ' X ' ' XX ' --token-chars '[ X]' foo && exit 1 || true
darcs replace $'A A'  'aaa' --token-chars '[^,]' foo && exit 1 || true
darcs replace $'A\tA' 'aaa' --token-chars '[^,]' foo && exit 1 || true
darcs replace $'A\vA' 'aaa' --token-chars '[^,]' foo && exit 1 || true

# Check that replace is not fooled by duplicate file names
# (i.e. not trying to performe the replace twice in the same file)
darcs replace X Y foo foo
darcs replace Y Z foo ../temp/foo
darcs replace Z Q foo foo --repodir=../temp/
darcs rec -am "xyzq"


# Try to "overwrite" a hunk with a replace.
#
# v1.0.8 accepts this without error or warning,
# but should perhaps require the --force option?
#
# current unstable sometimes(!) fails with bug: invalid pending
# which is surely a bug.

# this succeeds
echo "x" > foo
darcs rec -am xx
echo "y" > foo
darcs replace --ignore-times x y foo

# this fails
echo "hej" > foo
darcs rec -am hej
echo "hopp" > foo
darcs replace hej hopp foo

darcs whatsnew

echo "src" > foo
echo "dst" >> foo
darcs rec -am hop
darcs replace src dst foo || true
darcs replace --force src dst foo

darcs whatsnew
darcs whatsnew -ls

cd ..
rm -rf temp
