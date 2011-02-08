#!/usr/bin/env bash

## All these commands SHOULD fail (hence leading NOTs).
. lib

darcs show bug --debug 1> stdout 2> stderr || true

cat stdout
cat stderr

echo The following test will fail if this version of darcs is marked as
echo obsolete.
echo ==================================================================

not grep 'please do not' stderr

# The following test fails if HTTP isn't present, but would be a nice test
# to have in place.

#not grep unable stderr

grep 'fake bug' stderr
