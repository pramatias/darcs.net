#!/usr/bin/env bash
## Test for issue1879 - we should at least notice that when a patch claims
## to have the same identity (patchinfo) as one of ours, then it should not
## depend on anything we don't have.
##
## Public domain - 2010 Eric Kow

. lib                           # Load some portability helpers.
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.
darcs init      --repo S

cd R
touch x1
darcs add x1
darcs record -am 'x1'
darcs changes --context > ctx
echo hello > f
echo world > x1
darcs add f
darcs record -am 'hello world'
darcs send -a --context ctx -o foo.dpatch ../S
cd ..

cd S
touch x2
darcs add x2
darcs record -am 'x2'
darcs changes --context > ctx
# create an evil wrong patch
sed -e '/Context:/,$d' -e 's/x1/x2/g' ../R/foo.dpatch > foo.dpatch
cat ctx >> foo.dpatch
darcs apply foo.dpatch
cd ..

cd R
not darcs pull -a ../S 2>&1 | tee log
cd ..
