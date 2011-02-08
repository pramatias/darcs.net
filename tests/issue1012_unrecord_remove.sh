#!/usr/bin/env bash
set -ev
rm -rf temp1
mkdir temp1
cd temp1
darcs --version
darcs init
echo temp1 >File.hs
darcs add File.hs
darcs record File.hs -a -m "add File"
rm File.hs
darcs record -a -m "rm File"
darcs cha
darcs unrecord -p "rm File" -a
darcs cha
darcs record -a -m "re-rm File"
cd ..
rm -rf temp1
