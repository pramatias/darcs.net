#!/usr/bin/env bash
set -ev

# Test the --no-deps option with Send, Push, Pull, Obliterate and Unrecord.
#
# Create four patches with dependencies.
#   file 'f': patch 'fa' and 'fb'
#   file 'g': patch 'ga' and 'gb'
# The 'b' patches depend on the 'a' patches.

rm -rf tmp1
mkdir tmp1
cd tmp1
darcs init
echo 'record no-ask-deps' >> _darcs/prefs/defaults
echo 'record ignore-times' >> _darcs/prefs/defaults
echo 'a' > f
darcs add f
darcs rec -am 'fa' f
echo 'a' > g
darcs add g
darcs rec -am 'ga' g
echo 'b' > f
darcs rec -am 'fb' f
echo 'b' > g
darcs rec -am 'gb' g


mkdir d
darcs init --repodir d


# Try to Send all 'b' and 'g' patches. The two 'g' patches should succeed,
# but the 'fb' patch depends on the unselected 'fa' patch, an should be
# skipped.

darcs send -o bundle -a -p '[bg]' --no-deps d
grep '^\[ga$' bundle
grep '^\[fb$' bundle && exit 1


# Try to Push all 'b' and 'g' patches. Expect same result as for Send.

darcs push -a -p '[bg]' --no-deps d
cd d
darcs changes | grep '^  \* ga$'
darcs changes | grep '^  \* fb$' && exit 1
# stay in d !!


# Try to Pull all 'b' and 'g' patches. Expect same result as for Send.

# already in d
rm -rf *; darcs init
darcs pull -a -p '[bg]' --no-deps ..
darcs changes | grep '^  \* ga$'
darcs changes | grep '^  \* fb$' && exit 1
cd ..


# Try to Obliterate all 'a' and 'g' patches. The two 'g' patches should
# succeed, but the 'fa' patch depends on the unselected 'fb' patch, an
# should be skipped.

darcs get . tmp; cd tmp
echo y/y/y/q | tr / \\012 | darcs obliterate -p '[ag]' --no-deps
darcs changes | grep '^  \* gb$' && exit 1
darcs changes | grep '^  \* fa$'
cd ..


# Try to Unrecord all 'a' and 'g' patches. Expect same result as for
# Obliterate.

# in "top" tmp repo -- destroys it!
echo y/y/y/q | tr / \\012 | darcs unrecord -p '[ag]' --no-deps
darcs changes | grep '^  \* gb$' && exit 1
darcs changes | grep '^  \* fa$'

cd ..
rm -rf tmp1
