#!/bin/env bash
# A test for trackdown --bisect option.
# In general it construct various repositories and try
# to find the last recent failing patch and match it with
# expected position.
#
# It runs the same tests also for the trackdown without
# the --bisect.
################################################################

set -ev

if echo $OS | grep -i windows; then
    echo I do not know how to run a test program under windows
    exit 0
fi

ghc -o trackdown-bisect-helper $TESTBIN/trackdown-bisect-helper.hs

function make_repo_with_test {
    rm -fr temp1
    mkdir temp1 ; cd temp1 ; darcs init
    touch ./i
    touch ./j
    darcs add ./i
    darcs add ./j
    ../trackdown-bisect-helper $1
}

function cleanup_repo_after {
    cd ..
    rm -fr temp1
}

# You can remove --bisect for compare with linear trackdown
trackdown_args='--bisect' 

# Function return true if given patch was found.
# It expects that last line has finish with <SPACE><patchname>
# For the linear it is second last from the end, and last line
# is sentence if trackdown failed or succeed.
function is_found_good_patch  {
    if [ -z "$trackdown_args" ]; then
    tail -n 2 | grep " $1\$"
    else 
    tail -n 1 | grep " $1\$"
    fi
}

# Test command - Success condition is that file 'j' have one inside (1)
# That means if it has zero (0) it is failing test. 
test_cmd='grep -q 1 j'

#############################################################################
# Section with test-cases
#############################################################################

# TEST01: Repo with success in the half
testTrackdown() {
make_repo_with_test $1
if darcs trackdown $trackdown_args "$test_cmd" | is_found_good_patch $2; then
    echo "ok 1"
else
    echo "not ok 1. the trackdown should find last failing patch = $2."
    exit 1
fi
cleanup_repo_after
}

# TEST01: Repo with success in the half
test01() {
testTrackdown '[1,1,0,0,0]' 3
}

# TEST02: Repo with without success condition
test02() { 
testTrackdown '[0,0,0,0,0]' 1
}

# TEST03: Repo with with success condition at before last patch
test03() {
testTrackdown '[1,1,1,1,0]' 5
}

# TEST04: Repo with with success condition as first patch ever
test04() {
testTrackdown '[1,0,0,0,0]' 2
}

# TEST05: Long repo with with success condition as first patch ever
test05() {
testTrackdown '[1,0,0,0,0,0,0,0,0,0,0]' 2
}

# TEST06: Long repo with with success condition as sixth patch
test06() {
testTrackdown '[1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0]' 7
}

# TEST07: Long repo with with success condition very nead the head
test07() {
testTrackdown '[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0]' 54
}

# TEST08: Long repo with with success condition very nead the head
test08() {
testTrackdown '[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0]' 55
}

# TEST09: Long repo with non-monotone errors / success distribution
# This test only tests that it will not crash... 
test09() {
testTrackdown '[1,1,1,1,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0]' 7
}

#############################################
# call test-cases for trackdown linear
#############################################
trackdown_args=''
test01
test02
test03
test04
test05
test06
test07
test08
#############################################
# Call test-cases for trackdown bisect
#############################################
trackdown_args='--bisect' 
test01
test02
test03
test04
test05
test06
test07
test08
test09 # only for --bisect

