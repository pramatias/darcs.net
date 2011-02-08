#!/usr/bin/env bash

# Some tests for 'darcs record '

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init

# issue308 - no patches and no deps for record should abort
darcs record -am foo --ask-deps | grep -i "Ok, if you don't want to record anything, that's fine!"

# RT#476 - --ask-deps works when there are no patches
if echo $OS | grep -i windows; then
  echo This test does not work on Windows
else
  touch t.f
  darcs add t.f
  darcs record  -am add
  echo a | darcs record  -am foo --ask-deps | grep -i 'finished recording'
fi

# RT#231 - special message is given for nonexistent directories
not darcs record -am foo not_there.txt > log
grep -i 'not exist' log

# RT#231 - a nonexistent file before an existing file is handled correctly
touch b.t
darcs record  -lam foo a.t b.t > log
grep -i 'WARNING:.*a.t' log
grep -iv 'WARNING:.*b.t' log

DIR="`pwd`"
touch date.t
darcs add date.t
darcs record -a -m foo "$DIR/date.t" | grep -i 'finished recording'

# issue396 - record -l ""
touch 'notnull.t'
darcs record  -am foo -l "" notnull.t | grep -i 'finished recording'

# basic record
date >> date.t
darcs record -a -m basic_record date.t | grep -i 'finished recording'

# testing --logfile
date >> date.t
echo "second record\n" >> log.txt
darcs record  -a -m 'second record' --logfile=log.txt  date.t | grep -i 'finished recording'

cd ..
rm -rf temp1
