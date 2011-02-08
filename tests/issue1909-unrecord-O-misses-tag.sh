#!/usr/bin/env bash
## issue1909: unrecord -O in tagged repo makes a busted bundle

. lib

rm -rf R
mkdir R
darcs init --repo R
echo a > R/a
darcs rec -lam a --repo R --ignore-times
darcs tag -m T --repo R
echo b > R/a
darcs rec -lam b --repo R --ignore-times
echo c > R/a
darcs rec -lam c --repo R --ignore-times

darcs unpull -p c -a --repo R -O
cat c.dpatch
grep '^\[b' c.dpatch
grep TAG c.dpatch
