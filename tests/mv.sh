#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
echo hi world > temp.c
darcs add temp.c
darcs record --all -A test --name=hi
echo goodbye >> temp.c
darcs whatsnew
darcs record -a -A au -m bye
echo bar > bar.c
darcs add bar.c
darcs record -a -m one -A ex
darcs mv bar.c zig.c
darcs whatsnew
darcs record -a -m two -A ex
mkdir baz
darcs add baz
darcs whatsnew
darcs record -a -m three -A ex
darcs mv zig.c baz/bar.c
darcs whatsnew
darcs record -a -m four -A ex
darcs mv baz temp
darcs whatsnew
darcs record -a -m five -A ex

darcs mv temp temp 1> stdout 2> stderr || true
grep 'Cannot rename a file or directory onto itself' stderr

cd ..

rm -rf temp
mkdir temp
cd temp
darcs init
echo hi world > a
darcs add a
darcs record --all -m lower
cd ..
darcs get temp temp1
cd temp
darcs mv a A
echo goodbye > A
darcs record --all -m 'to upper'
cd ../temp1
darcs pull -a

cd ..
rm -rf temp temp1
