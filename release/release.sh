#!/bin/sh
set -e

VERSION=`grep -i ^version darcs.cabal | rev | cut -f1 -d' ' | rev`
echo version: $VERSION

if echo $VERSION | egrep -q '96|97|98|99'; then BETA=1; fi
BRANCH=`echo -n $VERSION | cut -d. -f1-2`

echo branch: $BRANCH

set -x

make website > /dev/null
darcs changes -t $VERSION
darcs changes -t $VERSION | grep -q $VERSION

## the beta releases have a different cabal name
if test -n "$BETA"; then
    cp darcs.cabal darcs-beta.cabal
    mv darcs.cabal darcs.cabal.orig
    sed -e "s,^Name: \\+darcs,Name: darcs-beta," -i'' darcs-beta.cabal
fi

runghc Setup sdist

if test -n "$BETA"; then
    mv darcs.cabal.orig darcs.cabal
    rm darcs-beta.cabal
    packagename=darcs-beta-$VERSION
else
    packagename=darcs-$VERSION
fi
tarballname="$packagename.tar.gz"

test -f dist/$tarballname
wd=`pwd`

cd /tmp
tar xvzf $wd/dist/$tarballname

# fix tarball permissions (Cabal bug 627)
# we can't use 'cabal sdist' as a workaround because we have our own sdist hook
find $packagename -type f -exec chmod 644 {} +
find $packagename -type d -exec chmod 755 {} +

tar czf $tarballname $packagename

test -f $tarballname
cp -f $tarballname $wd/dist/$tarballname

rm -rf $packagename

# okay, permissions fixed, test if we can build and test from the tarball
tar xvzf $wd/dist/$tarballname
cd $packagename

runghc Setup configure --user -ftype-witnesses -ftest
runghc Setup build
runghc Setup test
dist/build/unit/unit -j3 +RTS -N2

./dist/build/darcs/darcs --version
./dist/build/darcs/darcs --version | grep -q $VERSION
./dist/build/darcs/darcs --exact-version
./dist/build/darcs/darcs --exact-version | head -n 5 | grep -q $VERSION

cd ..
rm -rf $packagename
cd $wd

set +x

echo
echo ready: dist/$tarballname
echo
echo Next:
echo " darcs push to the public branch-$BRANCH repository"
echo " cabal upload dist/$tarballname"
