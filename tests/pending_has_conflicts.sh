#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

date > date.t
date > date_moved.t

write_buggy_pending () {
cat > _darcs/patches/pending <<EOF
{
addfile ./date.t
addfile ./date_moved.t
move ./date.t ./date_moved.t
}
EOF
}
write_buggy_pending

echo now watch the fireworks as all sorts of things fail
not darcs whatsnew 2>&1 | tee out
grep 'pending has conflicts' out

echo pending should now be fixed but there are no changes
not darcs whatsnew

write_buggy_pending

darcs revert -a 2>&1 | tee out
grep 'pending has conflicts' out

echo pending should now be emptied
darcs revert -a

write_buggy_pending

darcs record -a -m foo 2>&1 | tee out
grep 'pending has conflicts' out

darcs record -a -m foo

darcs changes -v

darcs repair 2>&1 | tee out
grep 'The repository is already consistent' out

write_buggy_pending

darcs repair 2>&1 | tee out
grep 'The repository is already consistent' out

cd ..
rm -rf temp1
