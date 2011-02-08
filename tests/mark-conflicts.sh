#!/usr/bin/env bash

# Automated tests for "darcs mark-conflicts".

# The builtin ! has the wrong semantics for not.
not () { "$@" && exit 1 || :; }

mkdir temp1
cd temp1
darcs init
echo "Conflict, Base ." > child_of_conflict
darcs add child_of_conflict
darcs record -am 'Conflict Base'
cd ..
darcs get temp1 temp2

# Add and record differing lines to both repos
cd temp1
echo "Conflict, Part 1." > child_of_conflict
darcs record -A author -am 'Conflict Part 1'
cd ..
cd temp2
echo "Conflict, Part 2." > child_of_conflict
darcs record -A author -am 'Conflict Part 2'
cd ..

cd temp1
darcs pull -a ../temp2 > log
grep conflict log
grep finished log
grep 'v v' child_of_conflict
darcs revert -a
not grep 'v v' child_of_conflict
darcs mark-conflicts
grep 'v v' child_of_conflict
cd ..

rm -rf temp1 temp2
