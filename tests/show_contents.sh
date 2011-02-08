#!/usr/bin/env bash
. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo
echo first > foo
darcs record -a -m "first edit" -A author1
echo second > foo
darcs record -a -m "second edit" -A author2
darcs tag t1 -A moi
echo third > foo
darcs record -a -m "third edit" -A author3
echo fourth > foo
darcs record -a -m "fourth edit" -A author4
echo unrecorded > foo
darcs show contents foo | grep fourth
darcs show contents foo -p third | grep third
darcs show contents foo --match="author author1" first | grep first
darcs show contents foo --tag t1 | grep second
not darcs show contents foo --match "hash bla" 2>&1 | tee out
grep "Couldn't match pattern" out
cd ..

rm -rf temp1
