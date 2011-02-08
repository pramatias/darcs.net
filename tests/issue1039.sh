#!/usr/bin/env bash


. lib

# pull from not empty repo to empty repo
rm -rf temp1 temp2
mkdir temp1 temp2

cd temp1
darcs init
echo a > a
darcs add a
darcs record --all --name=a
cd ../temp2
darcs init
darcs pull --all --dont-allow-conflicts ../temp1
test `darcs changes --count` = "1"
cd ..

# push from not empty repo to empty repo
rm -rf temp1 temp2
mkdir temp1 temp2

cd temp1
darcs init
cd ../temp2
darcs init
echo a > a
darcs add a
darcs record --all --name=a
darcs push --all ../temp1
cd ../temp1
test `darcs changes --count` = "1"
cd ..

# send from not empty repo to not empty repo
rm -rf temp1 temp2
mkdir temp1 temp2

cd temp1
darcs init
echo a > a
darcs add a
darcs record --all --name=a

for i in 1 2 3 4 5 6 7 8 9; do
    echo Change number $i >> a
    darcs record -a -m "change a $i"
done

cd ../temp2
darcs init
echo b > b
darcs add b
darcs record --all --name=b

for i in 1 2 3 4 5 6 7 8 9; do
    echo Change number $i >> b
    darcs record -a -m "change b $i"
done

echo no | darcs send --all --to=random@random --sendmail-command=false ../temp1
echo yes | not darcs send --all --to=random@random --sendmail-command=false ../temp1
not darcs send --all --to=random@random --sendmail-command=false --allow-unrelated-repos ../temp1
cd ..

rm -rf temp1 temp2
