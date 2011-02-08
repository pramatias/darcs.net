#!/usr/bin/env bash
set -ev

check_manifest () {
    : > files.tmp
    echo . > dirs.tmp
    echo . > files-dirs.tmp
    for x in $1 ; do
	echo "./$x" >> files.tmp
	echo "./$x" >> files-dirs.tmp
    done
    for x in $2 ; do
	echo "./$x" >> dirs.tmp
	echo "./$x" >> files-dirs.tmp
    done
    darcs query manifest $3 --files --no-directories > darcsraw-files.tmp
    darcs query manifest $3 --no-files --directories > darcsraw-dirs.tmp
    darcs query manifest $3 --files --directories > darcsraw-files-dirs.tmp
    for x in files dirs files-dirs ; do
        sort $x.tmp | sed -e 's,\\,/,' > expected-$x.tmp
        sort darcsraw-$x.tmp | sed -e 's,\\,/,' > darcs-$x.tmp
        diff -u expected-$x.tmp darcs-$x.tmp
    done }

rm -rf temp
mkdir temp
cd temp
darcs init

check_manifest "" "" "--no-pending"
check_manifest "" "" "--pending"
touch a b
darcs add a
check_manifest "" "" "--no-pending"
check_manifest "a" "" "--pending"
darcs add b
mkdir c
check_manifest "" "" "--no-pending"
check_manifest "a b" "" "--pending"
darcs add c
touch c/1 c/2
check_manifest "" "" "--no-pending"
check_manifest "a b" "c" "--pending"
darcs add c/1 c/2
check_manifest "" "" "--no-pending"
check_manifest "a b c/1 c/2" "c" "--pending"
mkdir d
touch d/3 d/4
darcs add d/3 d/4
check_manifest "" "" "--no-pending"
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--pending"
darcs record -A test --all --name "patch 1" --skip-long-comment
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--no-pending"
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--pending"

darcs mv d e
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--no-pending"
check_manifest "a b c/1 c/2 e/3 e/4" "c e" "--pending"
rm c/1
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--no-pending"
check_manifest "a b c/1 c/2 e/3 e/4" "c e" "--pending"
darcs remove c/1
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--no-pending"
check_manifest "a b c/2 e/3 e/4" "c e" "--pending"
darcs mv c/2 c/1
check_manifest "a b c/1 c/2 d/3 d/4" "c d" "--no-pending"
check_manifest "a b c/1 e/3 e/4" "c e" "--pending"
darcs record -A test --all --name "patch 2" --skip-long-comment
check_manifest "a b c/1 e/3 e/4" "c e" "--no-pending"
check_manifest "a b c/1 e/3 e/4" "c e" "--pending"

darcs remove c/1
check_manifest "a b c/1 e/3 e/4" "c e" "--no-pending"
check_manifest "a b e/3 e/4" "c e" "--pending"
darcs remove c
check_manifest "a b c/1 e/3 e/4" "c e" "--no-pending"
check_manifest "a b e/3 e/4" "e" "--pending"
darcs record -A test --all --name "patch 3" --skip-long-comment
check_manifest "a b e/3 e/4" "e" "--no-pending"
check_manifest "a b e/3 e/4" "e" "--pending"

darcs mv b b2
darcs mv b2 b3
check_manifest "a b e/3 e/4" "e" "--no-pending"
check_manifest "a b3 e/3 e/4" "e" "--pending"
darcs record -A test --all --name "patch 3" --skip-long-comment
check_manifest "a b3 e/3 e/4" "e" "--no-pending"
check_manifest "a b3 e/3 e/4" "e" "--pending"

cd ..
rm -rf temp
