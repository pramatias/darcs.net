#!/usr/bin/env bash

. lib


rm -rf temp1
mkdir temp1
cd temp1
darcs init

touch foo
darcs add foo

cat > log <<EOF
add file foo
Ignore-this: My private secret.
EOF

echo n | not darcs record -a --logfile log > out
cat out
grep Proceed out

darcs whatsnew

echo y | darcs record -a --logfile log

not darcs whatsnew

darcs changes > out
cat out
not grep 'My private secret' out

darcs changes --xml > out
cat out
grep 'My private secret' out

cd ..
rm -rf temp1
