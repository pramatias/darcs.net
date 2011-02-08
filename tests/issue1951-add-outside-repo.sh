#!/usr/bin/env bash
## Test for issue1951 - darcs should refuse adds from outside the
## current repository
##
## Copyright (C) 2010 Eric Kow
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

. lib                           # Load some portability helpers.

rm -rf R R2 R3 R4 R5 R6 R7 R8 R9 R10
darcs init      --repo R        # Create our test repos.
darcs init      --repo R2
darcs init      --repo R3
darcs init      --repo R4
darcs init      --repo R5
darcs init      --repo R6
darcs init      --repo R7
darcs init      --repo R8
darcs init      --repo R9
darcs init      --repo R10

# test if adding files outside the repository fails

OUTSIDE=`pwd`

cd R
echo 'Example content.' > f
echo 'Bad' > ../junk-for-issue1951
darcs add f
not darcs add ../junk-for-issue1951
not darcs add $OUTSIDE/junk-for-issue1951
darcs whatsnew > log
not grep junk-for-issue1951 log
cd ..

# test adding a file that:
#   * is in a subdirectory of the repository root
#   * is referred to with a path relative to the cwd, when the cwd is the
#     directory that the file is in
cd R2
mkdir subdir
cd subdir
touch f
darcs add f
darcs whatsnew > log
grep 'subdir/f' log
cd ../..

# same as above, but now the relative path is valid both from the cwd and from
# the repository root. Darcs should add the file in the cwd, not the one in the
# repository root
cd R3
touch f
mkdir subdir
cd subdir
touch f
darcs add f
darcs whatsnew > log
grep 'subdir/f' log
cd ../..

# now test that adding fails on a file that
#  * is in the repository root
#  * is referred to with a path relative to the repository root, when the cwd is
#    not the repository root
cd R4
touch myfilename
mkdir subdir
cd subdir
not darcs add myfilename
cd ../..

# test adding a file by relative path from the repo root, when the cwd is
# outside the repo
# It may seem counterintuitive that this succeeds and the cases above and below
# do not, but that's the way it is. We use this feature ourselves in our test
# suite.

touch R5/myfilename
darcs add --repo R5 myfilename
darcs whatsnew --repo R5 > log
grep 'myfilename' log
not grep '\.\./myfilename' log

# The case below makes the R4 case (of using a repo-root-relative path in a
# subdir of the repo) look even more like the R5 case (of using a
# repo-root-relative path outside the repo) by usign the --repo flag. It still
# failed on darcs 2.4.

cd R6
touch myfilename
mkdir subdir
cd subdir
not darcs add --repo .. myfilename
cd ../..

# Test adding a file by relative path from the repo root, when the cwd is
# outside the repo, and the relative path also exists from the cwd

touch myfilename
touch R7/myfilename
darcs add --repo R7 myfilename
darcs whatsnew --repo R7 > log
grep 'myfilename' log
not grep '\.\.myfilename' log

# Like the R4 case: try to use a path relative to the repo root from a cwd that
# is a subdir of the repo root. In this case the path relative to the repo root
# also includes a directory however.

cd R8
mkdir subdir1
mkdir subdir2
touch subdir2/myfilename
cd subdir1
not darcs add subdir2/myfilename
cd ../..

# Try adding a non-repository file using a non-existent repo subdirectory name
# followed by two ..'s
touch myfilename
cd R9
not darcs add nonexistentsubdir/../../myfilename
cd ..

# Try adding a non-repository file using an existing repo subdirectory name
# followed by two ..'s
touch myfilename
cd R10
mkdir subdir
not darcs add subdir/../../myfilename
cd ..
