#!/usr/bin/env bash

. lib


rm -rf temp1
mkdir temp1
cd temp1
darcs init

touch foo
darcs add foo
darcs record -a -m addfoo

darcs replace one two foo
darcs replace three four foo
darcs replace five six foo

echo sa | darcs record -m cancelled

darcs whatsnew

darcs changes > ch
not grep cancelled ch

cd ..
rm -rf temp1
