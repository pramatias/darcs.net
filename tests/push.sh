#!/usr/bin/env bash
set -ev

rm -rf temp temp_0
mkdir temp
cd temp
darcs init
echo tester > _darcs/prefs/author
date > bla
darcs add bla
darcs record -a --name=11
cd ..
darcs get temp
cd temp
date > bla2
darcs add bla2
darcs record -a --name=22
darcs push -a ../temp_0
cd ..

rm -rf temp temp_0
