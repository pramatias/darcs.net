#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs initialize --old-fashioned
echo A > foo
darcs add foo
darcs record -a -m AA -A x
echo '<UGLY HTML-LIKE GARBAGE RETURNED BY BAD HTTP SERVER>' > _darcs/format
cd ..

# ensure that we successfully get repositories even if they have a bogus
# format file, as can happen if no _darcs/format is present (i.e. it's
# generated by an older darcs) and an http server fails to produce a 404
# error code.  This is issue757.

darcs get temp1 temp2

echo intentional-error >> temp2/_darcs/format

cat temp2/_darcs/format

rm -rf temp1

not darcs get temp2 temp1 2> err

cat err
grep intentional-error err
grep 'understand repository format' err

rm -rf temp1 temp2
