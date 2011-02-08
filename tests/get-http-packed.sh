#!/usr/bin/env bash
# Written in 2010 by Petr Rockai, placed in public domain

. lib

rm -rf R S && mkdir R
cd R
darcs init

darcs show repo > repo
grep "Format: hashed" repo || exit 200 # not supported with oldfashioned repos

for f in `seq 1 200`; do
    echo $f > $f
    darcs rec -lam $f
done

darcs optimize # FIXME the pristine.hashed in the basic tarball has extra files
               # without this step

darcs optimize --http
test -e _darcs/packs/basic.tar.gz
test -e _darcs/packs/patches.tar.gz
cd ..

serve_http # sets baseurl
darcs get --packs $baseurl/R S
cd S
rm _darcs/prefs/sources # avoid any further contact with the original repository
darcs check
