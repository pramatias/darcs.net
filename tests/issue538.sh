#!/bin/env bash
# A test for issue 538 - that an executable test script will run successfully if
# it is recorded with --set-scripts-executable.

set -ev

if echo $OS | grep -i windows; then
    echo I do not know how to run a test program under windows
    exit 0
fi

function make_repo_with_test {
    mkdir temp1 ; cd temp1 ; darcs init
    echo "#!/bin/sh" > test.sh
    echo "echo 'hello world'" >> test.sh
    darcs add test.sh
    darcs record --author=test@test -am test
    darcs setpref test './test.sh'
}

# test record with --set-scripts-executable
rm -rf temp1
make_repo_with_test
touch blaat
darcs add blaat
if darcs record --set-scripts-executable -A test@test -am blaat --test; then
    echo "ok 1"
else
    echo "not ok 1 recording second patch failed (because test failed?)"
    exit 1
fi
cd ..

# test record without --set-scripts-executable 
rm -rf temp1
make_repo_with_test
touch blaat
darcs add blaat
if darcs record --dont-set-scripts-executable -A test@test -am blaat --test; then
    echo "not ok 2 recording second patch succeeded though test script should not be executable"
    exit 1
else
   echo "ok 2"
fi
cd ..

# test amend-record with --set-scripts-executable
rm -rf temp1
make_repo_with_test
touch blaat
darcs add blaat
if echo y | darcs amend-record --set-scripts-executable -A test@test -a --test; then
    echo "ok 3"
else
    echo "not ok 3 amending patch failed (because test failed?)"
    exit 1
fi
cd ..

# test amend-record without --set-scripts-executable
rm -rf temp1
make_repo_with_test
touch blaat
darcs add blaat
if echo y | darcs amend-record --dont-set-scripts-executable -A test@test -a /dev/null --test; then
    echo "not ok 4 amending patch succeeded even though --dont-set-scripts-executable specified"
    exit 1
else 
    echo "ok 4"
fi
cd ..

# trackdown with --set-scripts-executable
rm -rf temp1
make_repo_with_test
if darcs trackdown --set-scripts-executable | grep 'Success!' ; then
    echo "ok 5"
else
    echo "not ok 5 tracking down with --set-scripts-executable failed (because test failed?)"
    exit 1
fi
cd ..

# trackdown without --set-scripts-executable
rm -rf temp1
make_repo_with_test
if darcs trackdown --dont-set-scripts-executable | grep 'Noone passed the test!' ; then
    echo "ok 6"
else
    echo "not ok 6 tracking down did not find failure even though --dont-set-scripts-executable was given"
    exit 1
fi
cd ..

# check trackdown with files that become scripts during trackdown
rm -rf temp1
mkdir temp1 ; cd temp1 ; darcs init
echo "#!/bin/sh" > test.sh
echo "./helper.sh" >> test.sh
echo "#!/bin/sh" > helper.sh
echo "echo 'helper speaking'" >> helper.sh
darcs add test.sh
darcs add helper.sh
darcs record -am 'valid helper' -A test
echo 'this is definitely not a valid script' > helper.sh
darcs record -am 'invalid helper' -A test
darcs setpref test './test.sh'
darcs trackdown --set-scripts-executable > trackdown-out
if grep 'Test failed!' trackdown-out && grep 'Success!' trackdown-out ; then
    echo "ok 7"
else 
    echo "not ok 7 either no failure or no success (both should occur)"
    exit 1
fi
cd ..

rm -rf temp1
