#!/bin/sh

set -ev

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo first > a
darcs add a
darcs record --pipe --all --name=first <<EOF
Thu Sep 18 22:37:06 MSD 2008
author
EOF

echo first > b
darcs add b
darcs record --pipe --all --name=first <<EOF
Thu Sep 18 22:37:06 MSD 2008
author
EOF

# it seems that somehow the following occasionally fails on an nfs
# filesystem when darcs is compiled with ghc 6.6.

darcs get . ../temp2

# it fails with something like:

# darcs: ./_darcs/patches/20080918223706-f64cd-6b512400a8108808b7ba7057f0eac2adf473b3ae.gz: copyFile: resource busy (file is locked)


cd ..

rm -rf temp1 temp2
