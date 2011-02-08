#!/usr/bin/env bash
. lib

rm -rf temp1 temp2

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
echo | darcs pull -a ../temp2 --external-merge 'cp %2 %o'
cd ..

grep "Part 2" temp1/child_of_conflict
diff -u temp1/child_of_conflict temp2/child_of_conflict

cd temp1
darcs wh
darcs rev -a
echo y | darcs unpull --last 1 -a
echo | darcs pull -a ../temp2 --external-merge 'cp %1 %o'
cd ..

cat temp1/child_of_conflict
grep "Part 1" temp1/child_of_conflict

rm -rf temp1 temp2
