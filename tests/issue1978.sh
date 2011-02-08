#!/usr/bin/env bash

. lib

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
cat _darcs/format
cd ..

if grep 'old-fashioned' .darcs/defaults; then
    exit 200
fi

if grep hashed .darcs/defaults; then
    exit 200
fi

# get future repo: should be ok
darcs get future temp1
cd temp1
darcs changes
touch toto
darcs add toto
darcs record -am 'blah'
cd ..
rm -rf temp1 future

