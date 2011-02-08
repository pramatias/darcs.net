#!/usr/bin/env bash
set -ev

rm -rf temp temp2 temp3

#"$DARCS" get http://darcs.net temp

darcs get --lazy http://darcs.net temp2

darcs get --lazy --tag . http://darcs.net temp3

cd temp2
darcs obliterate --from-tag . -a
darcs pull --tag . -a
cd ..

diff -u temp2/_darcs/hashed_inventory temp3/_darcs/hashed_inventory

rm -rf temp temp2 temp3
