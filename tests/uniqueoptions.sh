#!/usr/bin/env bash
echo
echo Checking that each command expects each option only once
echo

set -ev

if echo $OS | grep -i windows; then
    echo Noone knows how to handle newlines under cygwin, so we skip this test
    exit 0
fi

rm -rf temp1
mkdir temp1
cd temp1

for i in `darcs --commands | grep -v -- -- | xargs echo`; do
    echo -n Checking $i... ' '
    # only output actual command options, i.e. lines that contain a --
    darcs $i --help | grep -- "--" | sort > $i
    uniq $i > uni$i
    if cmp $i uni$i; then
        echo passed.
    else
        echo failed!
        diff -c uni$i $i
        exit 1
    fi
done

cd ..
rm -rf temp1

