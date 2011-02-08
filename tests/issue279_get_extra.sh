#!/usr/bin/env bash

# issue279: a conflict case resulting in "bug in get_extra" with old
# format repos and "Malformed patch bundle" with darcs-2 repos.

. lib

rm -rf temp1 temp_a temp_b temp_c temp_d
mkdir temp1
cd temp1
darcs init --darcs-2
echo 0 > f
darcs add f
darcs record -am 00
cd ..

for r in a b c d; do
  darcs put --repodir temp1 temp_$r
  cd temp_$r;
  echo $r > f
  darcs record -am "patch:$r";
  cd ..
done

cd temp_d
darcs pull -a ../temp_a
darcs pull -a ../temp_b
darcs pull -a ../temp_c
cd ..
cd temp_c
darcs pull -a ../temp_a
darcs pull -a ../temp_b
echo rc > f
darcs record -a -m rc
cd ..
cd temp_d
darcs pull -a ../temp_c > log
not grep -i "no remote changes" log
not grep -i get_extra log
cd ..

rm -rf temp1 temp_a temp_b temp_c temp_d log
