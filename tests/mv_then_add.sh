#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs init
touch fee fi fo fum
darcs add f*
darcs record --author me --all --no-test --name add
darcs mv fee foo
touch fee
darcs add fee
darcs record --author me --all --no-test --name newfee
darcs mv fi fib
darcs record --author me --all --no-test --name mvfi
date > fi
darcs add fi 
darcs record --author me --all --no-test --name newfi

cd ..

rm -rf temp

