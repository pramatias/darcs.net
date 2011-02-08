#!/bin/bash

#  Issue578: A conflict rollback case for Darcs2

set -ev

rm -rf tmp_steve tmp_monica

mkdir tmp_steve
mkdir tmp_monica

cd tmp_steve
darcs init  --darcs-2
echo A >foo
darcs add foo
darcs record -Asteve -am 'Anote'

echo B >foo
darcs record -Asteve -am 'Bnote'

# Show the history as Steve sees it.
darcs changes -s
echo "######"

cd ../tmp_monica
darcs init --darcs-2
echo A>foo
darcs add foo
echo Z>bar
darcs add bar
darcs record -Amonica -am 'AZnote'
darcs pull -a ../tmp_steve
darcs changes
echo "######"
darcs rollback -a -m newpatch -A me --match 'exact Anote'

# previous failure result: darcs failed:  cannot roll back a 'rollback' patch.

