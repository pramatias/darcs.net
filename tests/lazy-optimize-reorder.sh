#!/usr/bin/env bash

. lib

rm -rf temp1 temp2 temp3
mkdir temp1
cd temp1
darcs init

# this test only applies to hashed formats
if cat _darcs/inventory; then exit 0; fi

date > f1
darcs add f1
darcs record -am 'add f1'

darcs tag -m 'tag f1'

date > f2
darcs add f2
darcs record -am 'add f2'
cd ..

darcs get --lazy temp1 temp2

darcs get --lazy temp2 temp3

cd temp2

# Run darcs changes so we pull in the inventories (but no the patches)
darcs changes

# Remove original repository, so we have no references to changes f1 and f2.
rm -rf ../temp1

# Now we should be unable to read some of the history
darcs changes -s > out
cat out
grep unavailable out

date > f3
darcs add f3
darcs record -am 'add f3'

darcs tag -m 'tag 123'
cd ..

cd temp3
date > f4
darcs add f4
darcs record -am 'add f4'
darcs pull -av

# Here's the point of this test: we should be able to optimize
# --reorder without being able to read all the patches in the
# repository.
darcs optimize --reorder

# Just a double-check: we shouldn't be able to check in this case.
not darcs check

cd ..

rm -rf temp1 temp2 temp3 temp4
