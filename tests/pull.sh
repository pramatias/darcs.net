#!/usr/bin/env bash

. lib

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init

cd ..
mkdir temp2
cd temp2
darcs init

mkdir one
cd one
mkdir two
cd two
echo darcs pull should work relative to the current directory
darcs pull -a ../../../temp1 | grep -i 'No remote changes to pull in'

echo -- darcs pull should pull into repo specified with --repo
cd ../..  # now in temp2
darcs add one;
darcs record --name uno --all
cd ..     # now outside of any repo
darcs pull --set-default --repodir temp1 --all temp2 | grep -i 'Finished pulling.' # temp2 is not relative to temp1

# set up server repo
date > temp2/one/date.t
darcs add --repodir ./temp2 one/date.t
darcs record --repodir ./temp2 -a -m foo

# set up client repo for failure
if echo $OS | grep -i windows; then
    echo this test does not work on windows because it
    echo is not possible to chmod -r
elif whoami | grep root; then
    echo root never gets permission denied
else
    chmod a-rwx ./temp1/one # remove all permissions
    not darcs pull --repodir ./temp1 -a 2> err
    chmod u+rwx temp1/one # restore permission
    cat err
    grep 'permission denied' err
    rm -rf temp1/one
fi

cd temp1

echo Before trying to pull from self, defaultrepo is something else
not grep temp1 _darcs/prefs/defaultrepo

#return special message when you try to pull from yourself
DIR="`pwd`"
not darcs pull --debug -a "$DIR" 2> out
cat out
grep 'Can.t pull from current repository' out

not darcs pull --debug -a . 2> out
cat out
grep 'Can.t pull from current repository' out

# and do not update the default repo to be the current di
not grep temp1 _darcs/prefs/defaultrepo

rm -f _darcs/prefs/defaultrepo
not darcs pull 2> err
grep 'please specify one' err
echo . > _darcs/prefs/defaultrepo
not darcs pull --debug 2> err
grep 'Can.t pull from current repository' err

not darcs pull --debug ../* 2> out
cat out
not grep 'Can.t pull from current repository' out
cd .. # now outside of any repo

cd temp1
echo a > foo
darcs record -lam AA
echo b > foo
darcs record -lam BB
echo c > foo
darcs record -lam CC
darcs rollback -p CC -a -m unC
cd ..
rm -rf temp2
darcs get --to-patch B temp1 temp2
cd temp2
sleep 1 # So that rollback won't have same timestamp as get.
darcs rollback -p BB -a -m unB
darcs revert -a
darcs pull -a ../temp1 2> err2
not grep 'Error applying patch' err2
cd ..

cd temp1
echo -n foo > baz
darcs add baz
darcs record  -am newbaz
cd ../temp2
darcs pull -a | grep Finished
echo -n bar > baz
darcs record  -am bazbar
cd ../temp1
darcs pull ../temp2 -a
echo -n bar > correct_baz
diff baz correct_baz
cd ..

#   my $test_name = "when a patch creating a directory is attempted to be applied
#       while a directory with that name already exists, a warning is raised, but
#       the pull succeeds.";
mkdir temp1/newdir
cd temp1
darcs add newdir
darcs record -am newdir
cd ../temp2
mkdir newdir
darcs pull -a --set-default ../temp1 &> out2
cat out
grep Backing out2
grep 'Finished pulling' out2
grep newdir out2
cd ..

rm -rf temp1 temp2


# A test for issue662, which triggered:
#  darcs failed:  Error applying hunk to file ./t.t
#  Error applying patch to the working directory.

rm -rf tmp;
darcs init --hashed --repodir=tmp
touch tmp/t.t
cd tmp
darcs add t.t
echo 'content'>t.t
darcs record -am 'initial add' --ignore
echo 'content: remote change'>t.t
darcs record -am 'remote change' --ignore
darcs put tmp2
cd tmp2
darcs obliterate --last 1 --all;
echo 'content: local change'> t.t
darcs pull -a ../
darcs w -s
darcs revert -a

cd ../..
rm -rf tmp
