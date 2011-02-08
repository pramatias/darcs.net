#!/usr/bin/env bash

# Issue1977 darcs repair complains when there is no pristine.hashed directory
set -ev
mkdir temp1
cd temp1
darcs init
echo "a" > a
darcs add a
darcs rec -am a
rm -rf _darcs/pristine.hashed/
darcs repair
