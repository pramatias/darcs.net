#!/usr/bin/env bash

. lib

rm -rf temp
mkdir temp
cd temp
darcs init
darcs setpref test false
darcs record --no-test -a -m 'add failing test'

# should pass with --no-test
darcs check --no-test

# should fail when test is run
not darcs check --test

# should pass with --no-test in defaults
echo check --no-test > _darcs/prefs/defaults
darcs check
not darcs check --test

# should fail with --test in defaults
echo check --test > _darcs/prefs/defaults
not darcs check
darcs check --no-test

# check global defaults
cp ~/.darcs/defaults defaults.backup
trap "cp defaults.backup ~/.darcs/defaults" EXIT
rm _darcs/prefs/defaults

# --no-test works in global defaults
echo check --no-test > ~/.darcs/defaults
darcs check
not darcs check --test

# --test works in global defaults
echo check --test > ~/.darcs/defaults
not darcs check
darcs check --no-test

# Verify that per-repository defaults override global defaults

# --no-test in repository defaults overrides global --test
echo check --test > ~/.darcs/defaults
echo check --no-test > _darcs/prefs/defaults
darcs check
not darcs check --test

# --test in repository defaults overrides global --no-test
echo check --no-test > ~/.darcs/defaults
echo check --test > _darcs/prefs/defaults
not darcs check
darcs check --no-test

trap - EXIT
cp defaults.backup ~/.darcs/defaults

cd ..
rm -rf temp
