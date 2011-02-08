#!/usr/bin/env bash
set -ev

darcs init
echo zig > foo
darcs add foo
darcs record -a -m add_foo -A x
echo zag >> foo
darcs record -a -m mod_foo -A y
echo bar > foo
darcs record -a -m mod2 -A y

darcs show authors > authors
grep x authors
grep y authors

head -1 authors > first-author
grep y first-author

darcs show authors --verbose
darcs show authors --verbose | grep y | wc -l > num-patches
cat num-patches
grep 2 num-patches
