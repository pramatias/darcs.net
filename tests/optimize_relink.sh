
#!/usr/bin/env bash

# For issue600, testing optimize --relink 

set -ev

## We don't support hard links on Windows.

if echo $OS | grep -i windows; then
    echo darcs does not support hard links on Windows
    exit 0
fi

## compare succeeds if there are hard links
compare () {
  echo 'use File::Basename; $res=0; while ($fn=<'$1'/*>) { $fn2="'$2'/" . basename($fn); @fd1=lstat($fn); @fd2=lstat($fn2); $res += ($fd1[1] != $fd2[1]);}; exit($res);' | perl
}

rm -rf temp
mkdir temp
cd temp

mkdir x
darcs init --repodir x
cd x
date > foo
darcs add foo
darcs record -a -A me -m 'addfoo'
cd ..

## Does the filesystem support hard linking at all?
mkdir z1
echo "hi" > z1/foo
mkdir z2
if ! ln z1/foo z2/foo ; then
  echo No ln command for `pwd` - assuming no hard links.
  exit 0
fi
if ! compare z1 z2 ; then
  echo Filesystem for `pwd` does not support hard links.
  exit 0
fi
# workaround for SunOS cp which does not support `-a' option but also
# doesn't fail when it is encountered.
cp -r x y

## Now try relinking using darcs.
rm -rf z
darcs optimize --verbose --relink --repodir x --sibling y
rm -rf x/_darcs/patches/pend* y/_darcs/patches/pend*
if compare x/_darcs/patches y/_darcs/patches
then echo darcs --relink is working, hard links were done.
else echo darcs --relink is not working, it did not make any hard links.
     exit 2
fi

cd ..
rm -rf temp
