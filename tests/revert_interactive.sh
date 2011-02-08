#!/usr/bin/env bash
set -ev

rm -rf temp1
mkdir temp1
cd temp1
darcs init
echo hello world > foo
darcs add foo
darcs record -a -m add -A x
echo goodbye world >> foo
echo y/y | tr / \\012 | darcs revert
darcs show contents foo | cmp foo -

# Now let's test a trickier revert where changes commute nontrivially.

cat > foo <<EOF
a
b
c
d
e
EOF

darcs record -a -A me -m cleanup

mv foo foo.tmp
cat foo.tmp | grep -v b | grep -v d > foo

echo "nyy" | darcs revert

DARCS_DONT_COLOR=1 darcs wh > whatsnew
cat > correct <<EOF
hunk ./foo 2
-b
EOF
diff -c correct whatsnew

# Try a situation where earlier (kept) changes are depended upon by the
# changes we want to revert:

darcs record -a -A me -m cleanup

echo hello world > bar
echo hello world > foo
darcs add bar
darcs replace hello goodbye bar foo

echo "cnnnyy/y" | tr / \\012 | darcs revert

DARCS_DONT_COLOR=1 darcs wh > whatsnew
cat > correct <<EOF
addfile ./bar
hunk ./bar 1
+goodbye world
hunk ./foo 1
-a
-c
-d
-e
+hello world
EOF
diff -c correct whatsnew

cd ..
rm -rf temp1

