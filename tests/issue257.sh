#!/usr/bin/env bash
. lib

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
