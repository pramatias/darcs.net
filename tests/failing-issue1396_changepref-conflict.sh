#!/usr/bin/env bash
set -ev

. lib

rm -rf temp1 temp1a temp1b

mkdir temp1
cd temp1
darcs init
darcs setpref test 'echo nothing'
darcs record -am 'null pref'
cd ..

darcs get temp1 temp1a
cd temp1a
darcs setpref test 'echo a'
darcs record -am 'pref a'
cd ..

darcs get temp1 temp1b
cd temp1b
darcs setpref test 'echo b'
darcs record -am 'pref b'
cd ..

cd temp1
darcs pull -a ../temp1a --dont-allow-conflicts
not darcs pull -a ../temp1b --dont-allow-conflicts
cd ..

rm -rf temp1 temp1a temp1b
