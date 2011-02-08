#!/usr/bin/env bash

. lib

rm -rf temp1 temp2

# this should fail, since temp1 doesn't exist...
not darcs get temp1 temp2

# verify that temp2 wasn't created
not cd temp2

rm -rf temp1 temp2
