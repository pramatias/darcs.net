#!/usr/bin/env bash

. lib

rm -rf garbage future
mkdir garbage
cd garbage
darcs  init
echo gobbledygook > _darcs/format
cd ..

mkdir future
cd future
darcs  init
touch titi
darcs add titi
darcs record -am titi
cat > _darcs/format <<EOF
hashed|gobbledygook
darcs-2
EOF
cd ..


# check the rules for reading and writing

## garbage repo: we don't understand anything
# get garbage repo
not darcs get garbage temp1 2> log
grep -i "can't understand repository format" log
rm -rf temp1 log

# pull from garbage repo
mkdir temp1
cd temp1
darcs init
not darcs pull ../garbage 2> log
grep -i "can't understand repository format" log
cd ..
rm -rf temp1

# apply in garbage repo
mkdir temp1
cd temp1
darcs init
darcs changes --context > empty-context
darcs tag -m "just a patch"
darcs send -a --context=empty-context -o ../bundle.dpatch .
cd ../garbage
not darcs apply ../bundle.dpatch 2> log
grep -i "can't understand repository format" log
cd ..
rm -rf temp1 bundle.dpatch

# add in garbage repo
cd garbage
touch toto
not darcs add toto 2> log
grep -i "can't understand repository format" log
cd ..

rm -rf garbage


## future repo: we don't understand one
#  alternative of a line of format
#  only look at future vs darcs2

if grep 'old-fashioned' .darcs/defaults; then
    exit 200
fi

if grep hashed .darcs/defaults; then
    exit 200
fi

# get future repo: ok
# --to-match is needed because of bug###
darcs get future temp1 --to-match "name titi"
cd temp1
darcs changes
touch toto
darcs add toto
darcs record -am 'blah'
cd ..
rm -rf temp1

# pull from future repo: ok
mkdir temp1
cd temp1
darcs init
darcs pull ../future -a
darcs cha | grep titi
cd ..
rm -rf temp1

# apply in future repo: !ok
mkdir temp1
cd temp1
darcs init
darcs changes --context > empty-context
darcs tag -m "just a patch"
darcs send -a --context=empty-context -o ../bundle.dpatch .
cd ../future
not darcs apply ../bundle.dpatch 2> log
cat log
grep -i "can't write repository format" log
cd ..
rm -rf temp1 bundle.dpatch

# record in future repo: !ok
cd future
touch toto
not darcs add toto 2> log
grep -i "can't write repository format" log
cd ..

rm -rf future #No future!
