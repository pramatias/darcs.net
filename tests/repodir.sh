#!/usr/bin/env bash
### http://bugs.darcs.net/issue496
### _darcs/prefs/defaults ignored when using --repodir

## All these commands SHOULD fail (hence leading NOTs).
. lib

rm -rf temp
mkdir temp
cd temp

mkdir repo
darcs initialize --repodir repo
cd repo
date > foo
darcs add foo
darcs record -a -m auth

echo > _darcs/prefs/defaults ALL disable # try to disable all
not darcs changes
not darcs changes --repodir "`pwd`"

cd ..

not darcs changes --repodir repo
not darcs changes --repodir "`pwd`/repo"

rm -rf temp
