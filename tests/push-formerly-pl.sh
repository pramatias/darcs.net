#!/usr/bin/env bash

# Some tests for 'darcs push'

. lib

slash() {
if echo $OS | grep -q -i windows; then
    echo -n \\
else
    echo -n /
fi
}

DIR="`pwd`"

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
cd ..
mkdir temp2
cd temp2
darcs init
cd ..

# push without a repo gives an error
cd temp1
not darcs push -p 123 2> log
grep -i 'missing argument' log
cd ..

mkdir -p temp2/one/two
cd temp2/one/two
# darcs push should work relative to the current directory
darcs push -a ../../../temp1 | grep -i 'No recorded local changes to push'
cd ../../../

# darcs push should push into repo specified with --repo
cd temp2
darcs add one
darcs record --name uno --all
cd ..

darcs push --repodir temp2 --all temp1 | grep -i 'Finished apply'

cd temp1
# Before trying to pull from self, defaultrepo does not exist
test ! -e _darcs/prefs/defaultrepo
# return special message when you try to push to yourself
not darcs push -a "$DIR`slash`temp1" 2> log
grep -i "cannot push from repository to itself" log
# and don't update the default repo to be the current dir
test ! -e _darcs/prefs/defaultrepo
cd ..

rm -rf temp1 temp2
