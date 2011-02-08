#!/usr/bin/env bash
set -ev

rm -rf temp1 temp2
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
cp foo foo_version_1.0
echo D > foo
darcs record -a -m DD -A x
echo E > foo
darcs record -a -m EE -A x
echo F > foo
darcs record -a -m FF -A x


# Check that get store commuted patches
cd ..
darcs get --tag 1.0 --repo-name temp2 temp1
cmp temp2/foo temp1/foo_version_1.0
rm -rf temp1 temp2 temp3

mkdir temp1
cd temp1
darcs init
cat > file <<EOF
1
2
3
4
EOF
darcs rec -Ax -alm 'Add file'
cat > file <<EOF
2
3
4
EOF
darcs rec -Ax -alm 'Remove line 1'
cat > file <<EOF
2
4
EOF
darcs rec -Ax -alm 'Remove line 3'
cd ..
mkdir temp2
cd temp2
darcs init
echo y/n/y | tr / \\012 | darcs pull ../temp1
darcs tag -Ax -m Tag
darcs push -a ../temp1
cd ..
darcs get --tag=Tag temp1 temp3
cd temp3
darcs check
cd ..
rm -rf temp1 temp2 temp3

# Check that pending looks ok
mkdir temp1
cd temp1
darcs init
mkdir d
darcs add d
darcs rec -Ax -am 'add d'
darcs tag -Ax t
rmdir d
darcs rec -Ax -am 'rm d'
cd ..
darcs get --tag t temp1 temp2
cd temp2
if [ -f _darcs/patches/pending ]; then
	if grep -v '^[{}]$' _darcs/patches/pending >/dev/null; then
		cat _darcs/patches/pending
		exit 1
	fi
fi
cd ..
rm -rf temp1 temp2
