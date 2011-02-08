#!/usr/bin/env bash
## Test for issue1248 - darcs doesn't handle darcs 1 repos with compressed
## inventories
##
## Placed into the public domain by Ganesh Sittampalam, 2009

. lib
rm -rf temp1
mkdir temp1
cd temp1
darcs init --old-fashioned
touch foo
darcs rec -a -m'foo' -l --author=me@me
darcs tag --author=me@me foo
darcs check
darcs optimize --reorder
darcs check
darcs optimize --compress
darcs check
cd ..
rm -rf temp1
