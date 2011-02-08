#!/usr/bin/env bash

set -ev
rm -rf temp1 temp2

# creating the fork point
mkdir temp1
cd temp1
darcs init
cat > foo << FOO
original - apple
original - banana
FOO
darcs add foo
darcs record -am init
cd ..
darcs get temp1 temp2

# do some work in the mainline
cd temp1
cat > foo << FOO
conflict 1 - artichoke
original - banana
FOO
darcs record -am 'conflict 1a'
cat > foo << FOO
conflict 1 - artichoke
conflict 1 - brocolli
FOO
darcs record -am 'conflict 1b'
cd ..

# do some work in the branch
cd temp2
cat > foo << FOO
conflict 2 - aardvark
original - banana
conflict 2 - cougar
FOO
darcs record -am 'conflict 2'
cd ..

# in the branch, pull from the mainline and resolve the conflict
cd temp2
darcs pull -a ../temp1 --allow-conflicts
cat > foo << FOO
resolution
original - apple
original - banana
FOO
darcs record -am 'resolve conflicts 2,1a,1b'
cd ..

# do some extra work in the mainline
cd temp1
cat > foo << FOO
original - apple
FOO
darcs record -am 'conflict 1c'
cd ..

# in the branch, pull from the mainline again
cd temp2
darcs pull -a ../temp1 --allow-conflicts
cd ..

rm -rf temp1 temp2
