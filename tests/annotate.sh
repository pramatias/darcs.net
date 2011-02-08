#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp
darcs init
mkdir a b
touch a/a b/b
darcs add --rec .
darcs record -a -m ab -A test
darcs annotate a/a
# annotate --xml should encode angle brackets in user name
touch c
darcs add c
darcs record -a -m foo -A 'Mark Stosberg <a@b.com>'
darcs annotate --xml c | grep "&lt;a\@b.com&gt;"
cd ..
rm -rf temp
