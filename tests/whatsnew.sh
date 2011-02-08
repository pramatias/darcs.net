#!/usr/bin/env bash

. lib

# Some tests for 'darcs whatsnew '

rm -rf temp1 temp2

mkdir temp1
cd temp1

# RT#505 whatsnew -s after removal of file without a newline
darcs init
echo -n foobar > foo
darcs record -la -m "add foo" | grep "Finished record"
rm -f foo
darcs whatsnew -s | grep R
darcs record -a -m "remove foo"

# RT#245 --look-for-adds implies --summary
touch look_summary.txt
darcs whatsnew -l | grep -i "a ./look_summary.txt"

#whatsnew works with uncommon file names
if echo $OS | grep -i windows; then
  echo  test does not work on windows
  exit 0;
else
  echo foo > \\
  darcs add \\
  darcs whatsnew | tee log
  grep 'hunk ./\\92\\' log
fi

echo foo > "foo bar"
darcs add "foo bar"
darcs wh | tee log
grep 'hunk ./foo\\32\\bar' log

# check that filename encoding does not botch up the index
darcs rec -am "weird filenames"
not darcs wh

# whatsnew works with absolute paths
DIR="`pwd`"
echo date.t > date.t
touch date.t
darcs add date.t
darcs whatsnew "$DIR/date.t" | grep hunk

cd ..

rm -rf temp1
