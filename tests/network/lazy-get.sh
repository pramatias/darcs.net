#!/usr/bin/env bash
set -ev

rm -rf temp temp2 temp3

darcs get --lazy http://darcs.net/repos/franchise temp

darcs get --lazy temp temp2

rm -rf temp

cd temp2

test ! -f _darcs/patches/0000003700-fb09ec6a61b35ac7aa77e65388b1a0c182fddca9a908f1a08d0bc5653f2f2d2a

darcs annotate -p 'initial minimal version'

test -f _darcs/patches/0000003700-fb09ec6a61b35ac7aa77e65388b1a0c182fddca9a908f1a08d0bc5653f2f2d2a

cd ..

rm -rf temp temp2 temp3
