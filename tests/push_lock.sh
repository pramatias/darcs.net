#!/usr/bin/env bash

# For issue257: push => incorrect return code when couldn't get lock

set -ev

rm -rf tempc
mkdir tempc
cd tempc
darcs init
echo foo > foo.c
darcs rec -Ax -alm init
cd ..
rm -rf temps
darcs get tempc temps
cd temps
echo server >> foo.c
darcs rec -Ax -alm server
cd ../tempc
echo client >> foo.c
darcs rec -Ax -alm client
if darcs push -a ../temps; then
        false
fi
cd ..
rm -rf tempc temps
