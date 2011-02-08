#!/usr/bin/env bash
set -ev

# Note that this is fixed, the lines marked # BUG HERE
# should be moved back into merging_newlines.sh

# trick: requiring something to fail
not () { "$@" && exit 1 || :; }

rm -rf temp1 temp2

# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

cd temp1
echo "apply allow-conflicts" > _darcs/prefs/defaults
# note: to make this pass, change echo to echo -n
# is that right?
echo "from temp1" > one.txt
darcs add one.txt
darcs record -A bar -am "add one.txt"
echo >> one.txt
darcs wh -u
cd ..

darcs get temp1 temp2
cd temp2
# reality check
darcs show files | grep one.txt
echo "in tmp2" >> one.txt
darcs whatsnew -s | grep M
darcs record -A bar -am "add extra line"
darcs annotate -p . -u
darcs push -av > log
cat log
not grep -i conflicts log
# BUG HERE
# after a conflict, darcs resolve should report a conflict
darcs mark-conflicts > log 2>&1
cat log
not grep -i 'no conflicts' log
cd ..

rm -rf temp1 temp2
