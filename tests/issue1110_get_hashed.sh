#!/usr/bin/env bash
set -ev
rm -rf temp1 temp1-h
mkdir temp1
cd temp1
darcs init --old-fashioned-inventory
cd ..
darcs get --hashed temp1 temp1-h
test -e temp1-h/_darcs/hashed_inventory
rm -rf temp1 temp1-h
