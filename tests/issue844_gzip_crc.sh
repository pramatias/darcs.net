#!/usr/bin/env bash
set -ev
rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
echo > a
darcs add a
darcs record a -a -m "init"
cd ..

mkdir temp2
cd temp2
darcs init
darcs pull ../temp1 -a
darcs optimize --compress
for f in _darcs/patches/*-*; do
	gzip -t < "$f"
done
rm -rf temp1 temp2
