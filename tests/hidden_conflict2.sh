#!/usr/bin/env bash
set -ev

# A test for a missed resolution, inspired by bug #10 in RT

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
cd ..
mkdir temp2
cd temp2
darcs init
cd ..

# set up temp1
cd temp1
cat > A << FOO
i

m
b
v
FOO
darcs add A
darcs record -m 'add' --all
cd ..

# set up temp2
cd temp2
darcs pull --all ../temp1
cat > A << FOO
J
i

C2

m
D
b
v
FOO
darcs record  -m 'change2' --all
cd ..

# generate a conflict
cd temp1
cat > A << FOO
I
i

C1

m
b

FOO
darcs record -m 'change1' --all
darcs pull --all ../temp2
# we should have a marked conflict now.
grep 'v v' A
# we resolve it simply by removing conflict markers.
sed -e '/\^ \^\|\*\*\|v v/d' A > temp
mv temp A
darcs record -m resolution --all
# now mark-conflicts shouldn't find any unmarked conflicts
darcs mark-conflicts | grep "No conflicts to mark"
cd ..

rm -rf temp1 temp2
