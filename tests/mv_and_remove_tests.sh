#!/usr/bin/env bash
set -ev

rm -rf temp
mkdir temp
cd temp

darcs init
touch fee fi fo fum
darcs add f*
darcs record --author me --all --no-test --name add
mkdir d
darcs add d
darcs mv f* d
darcs remove d/fi
cd d
darcs remove fo
echo let us have fun > fun
darcs add fun
darcs mv fun fum ..
darcs record --author me --all --no-test --name mv
cd ..

if darcs show files | egrep '^./fee$'; then false; else true; fi
test ! -f fee
darcs show contents d/fee | cmp d/fee -

test ! -f fi
test -f d/fi
if darcs show files | egrep '^./fi$'; then false; else true; fi
if darcs show files | egrep '^./d/fi$'; then false; else true; fi

test ! -f fo
test -f d/fo
if darcs show files | egrep '^./fo$'; then false; else true; fi
if darcs show files | egrep '^./d/fo$'; then false; else true; fi

darcs show contents fun | cmp fun -
darcs show contents fum | cmp fum -

darcs mv fun d
darcs record -A me -a --no-test -m "fun again"
darcs show content d/fun | cmp d/fun -
test ! -f fun
if darcs show files | egrep '^./fun$'; then false; else true; fi

# Now clean up.
cd ..
rm -rf temp

