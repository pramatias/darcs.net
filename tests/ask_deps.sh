#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs init
cat > _darcs/prefs/defaults <<.
ALL author test
ALL ignore-times
ALL ask-deps
.

# add three depending patches for file 'a'
# expect no dependency questions
# 'q' will abort and cause future failure if an unexpected dependency is asked
touch a
darcs add a
echo q | darcs rec -am a0
darcs ann -p a0
echo 1 > a
echo q | darcs rec -am a1
darcs ann -p a1
echo 2 > a
echo q | darcs rec -am a2
darcs ann -p a2

# add some patches for file 'b'
# expect no dependency questions for file 'b',
# but every time expect questions for the three patches of file 'a'
# every 'n' should continue to ask about the next patch
# the first 'y' should make all following dependencies of 'a' implicit and stop asking
# 'q' will abort and cause future failure if an unexpected dependency is asked
touch b
darcs add b
# test 0
echo n/n/n/q | tr / \\012 | darcs rec -am b0
darcs ann -p b0
# test 1
echo 1 > b
echo n/n/y/q | tr / \\012 | darcs rec -am b1
darcs ann -p b1 | grep '^\[a0'
# test 2
echo 2 > b
echo n/y/q | tr / \\012 | darcs rec -am b2
darcs ann -p b2 | grep '^\[a1'
# test 3
echo 3 > b
echo y/q | tr / \\012 | darcs rec -am b3
darcs ann -p b3 | grep '^\[a2'

cd ..
rm -rf temp

