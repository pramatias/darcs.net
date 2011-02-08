#!/usr/bin/env bash

# issue157: A test for how rollback handles dependencies and conflicts.

. lib

rm -rf temp1 temp2

# set up the repository
mkdir temp1
cd temp1
darcs init --darcs-2
cd ..

# do some work here
cd temp1
echo a > foo
# Create three patches
darcs record -lam AA
echo b > foo
darcs record  -lam BB
echo c > foo
darcs record  -lam CC
# Rollback the last one
darcs rollback -a -p CC -m 'rollback CC'
grep b foo # reality check
cd ..

# Now get just the first two patches into a new repo...ignoring the third patch and the rollback.
darcs get --to-patch BB temp1 temp2
cd temp2
# Now in the second repo, rollback the second patch.
darcs rollback -a -p BB -m 'rollback BB'
grep a foo # reality check
# Finally, pull the third patch and the rollback of it.
# Because the two patches should cancel each other other, there should be no change here.
darcs pull -a ../temp1
# expect a conflict between the contents being 'a' or 'b'
grep "v v v" foo
grep a foo
grep b foo
cd ..

rm -rf temp1 temp2
