#!/usr/bin/env bash
## Test for issue1702 - an optimize --relink does not relink the files
## in ~/.darcs/cache.
##
## Copyright (C) 2009  Trent W. Buck
##
## Permission is hereby granted, free of charge, to any person
## obtaining a copy of this software and associated documentation
## files (the "Software"), to deal in the Software without
## restriction, including without limitation the rights to use, copy,
## modify, merge, publish, distribute, sublicense, and/or sell copies
## of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be
## included in all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
## EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
## MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
## NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
## BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
## ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
## CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
## SOFTWARE.

. lib                  # Load some portability helpers.
rm -rf R S                      # Another script may have left a mess.
darcs init      --repo R        # Create our test repos.

## Create a patch.
echo 'Example content.' > R/f
darcs record -lam 'Add f.' --repodir R

## Get a hard link into the cache.
darcs get R S

## Are hard links available?
x=(R/_darcs/patches/*-*)
x=${x#R/_darcs/patches/}
if [[ ! R/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]]
then
    echo This test requires filesystem support for hard links.
    echo This test requires the hashed (or darcs-2) repo format.
    exit 200
fi

## IMPORTANT!  In bash [[ ]] is neither a builtin nor a command; it is
## a keyword.  This means it can fail without tripping ./lib's set -e.
## This is why all invocations below have the form [[ ... ]] || false.

## Confirm that all three are hard linked.
ls -lids {~/.darcs/cache,[RS]/_darcs}/patches/$x # debugging
[[ R/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ S/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ R/_darcs/patches/$x -ef S/_darcs/patches/$x ]] || false

## Break all hard links.
rm -rf S
cp -r R S
rm -rf R
cp -r S R

## Confirm that there are no hard links.
ls -lids {~/.darcs/cache,[RS]/_darcs}/patches/$x # debugging
[[ ! R/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ ! S/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ ! R/_darcs/patches/$x -ef S/_darcs/patches/$x ]] || false

## Optimize *should* hard-link all three together.
darcs optimize --relink --repodir R --sibling S

## Confirm that all three are hard linked.
ls -lids {~/.darcs/cache,[RS]/_darcs}/patches/$x # debugging
[[ R/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ S/_darcs/patches/$x -ef ~/.darcs/cache/patches/$x ]] || false
[[ R/_darcs/patches/$x -ef S/_darcs/patches/$x ]] || false
