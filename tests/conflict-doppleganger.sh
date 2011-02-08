#!/usr/bin/env bash

. lib

# Tests for the doppleganger conflict bug.

# For Zooko, with love
# Also, for issue81.

rm -rf temp
mkdir temp
cd temp

# check that dopplegangers conflict in --old-fashioned
rm -rf tmp_dopple tmp_ganger
mkdir tmp_dopple
cd tmp_dopple
darcs init --old-fashioned

darcs show repo | grep darcs-1.0
touch a.txt
darcs add a.txt
darcs record -A base -am 'adding a.txt'
cd ..

darcs get tmp_dopple tmp_ganger

for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    cd $repo
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
grep 'conflict' out

# check that dopplegangers conflict in --hashed
rm -rf tmp_dopple tmp_ganger
mkdir tmp_dopple
cd tmp_dopple
darcs init --hashed

darcs show repo | grep hashed
touch a.txt
darcs add a.txt
darcs record -A base -am 'adding a.txt'
cd ..

darcs get tmp_dopple tmp_ganger

for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    cd $repo
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
grep 'conflict' out
cd ..

# check that dopplegangers do not conflict in --darcs-2
rm -rf tmp_dopple tmp_ganger
mkdir tmp_dopple
cd tmp_dopple
darcs init --darcs-2

darcs show repo | grep darcs-2
touch a.txt
darcs add a.txt
darcs record -A base -am 'adding a.txt'
cd ..

darcs get tmp_dopple tmp_ganger

for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    cd $repo
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
not grep 'conflict' out
cd ..

# Checking that resolution dopplegangers conflict in --old-fashioned-inventory
rm -rf temp0 temp1 temp2 tmp_dopple tmp_ganger
mkdir temp0
cd temp0
darcs init --old-fashioned-inventory
darcs show repo | grep darcs-1.0
cd ..

# Create a conflict
darcs get --old-fashioned-inventory temp0 temp1
cd temp1
darcs show repo
darcs show repo | grep darcs-1.0
echo temp1 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp1 a.txt'
cd ..
darcs get temp0 temp2
cd temp2
echo temp2 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp2 a.txt'
cd ..

# Resolve the conflict the same way on both sides
for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    darcs get temp1 $repo
    cd $repo
    darcs pull -a ../temp2
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
grep conflict out
cd ..

# Checking that resolution dopplegangers conflict in --hashed
rm -rf temp0 temp1 temp2 tmp_dopple tmp_ganger
mkdir temp0
cd temp0
darcs init --hashed
darcs show repo | grep hashed
cd ..

# Create a conflict
darcs get temp0 temp1
cd temp1
echo temp1 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp1 a.txt'
cd ..
darcs get temp0 temp2
cd temp2
echo temp2 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp2 a.txt'
cd ..

# Resolve the conflict the same way on both sides
for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    darcs get temp1 $repo
    cd $repo
    darcs pull -a ../temp2
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
grep conflict out
cd ..


# Checking that resolution dopplegangers do not conflict in --darcs-2
rm -rf temp0 temp1 temp2 tmp_dopple tmp_ganger
mkdir temp0
cd temp0
darcs init --darcs-2
darcs show repo | grep darcs-2
cd ..

# Create a conflict
darcs get temp0 temp1
cd temp1
darcs show repo | grep darcs-2
echo temp1 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp1 a.txt'
cd ..
darcs get temp0 temp2
cd temp2
echo temp2 > a.txt
darcs add a.txt
darcs record  -A base -am 'adding temp2 a.txt'
cd ..

# Resolve the conflict the same way on both sides
for repo in tmp_dopple tmp_ganger; do
    echo working on $repo
    darcs get temp1 $repo
    cd $repo
    darcs pull -a ../temp2
    echo "text which appears in both places at once" > a.txt
    darcs record -A $repo -am "recording an identical change in $repo"
    cd ..
done

# Now that the conflict has been set up, try pull one patch from the other.
cd tmp_ganger
darcs pull -a ../tmp_dopple > out
cat out
not grep conflict out
cd ..

cd ..
rm -rf temp
