#!/usr/bin/env bash
set -ev

# Set up a base repo. Our experiment will start from this point
mkdir base
cd base
darcs init --darcs-2
printf "Line1\nLine2\nLine3\n" > foo
darcs rec -alm Base
cd ..

# Now we want to record patch A, which will turn "Line2" into "Hello"
darcs get base a
cd a
printf "Line1\nHello\nLine3\n" > foo
darcs rec --ignore-times -am A
cd ..

# Make B the same as A
darcs get base b
cd b
printf "Line1\nHello\nLine3\n" > foo
darcs rec --ignore-times -am B
cd ..

# Now we make a patch C that depends on A
darcs get a ac
cd ac
printf "Line1\nWorld\nLine3\n" > foo
darcs rec --ignore-times -am C
cd ..

# Merge A and B
darcs get a ab
cd ab
darcs pull -a ../b
darcs revert -a
cd ..

# And merge in C too
darcs get ab abc
cd abc
darcs pull -a ../ac
darcs revert -a
cd ..

# Now we can pull just B and C into base
darcs get base bc
cd bc
darcs pull ../abc -ap 'B|C'
cd ..

# Now we have base, B and C in a repository.  At this point we're correct.

# Let's try merging AC with BC now, here we discover a bug.

darcs get ac abc2
cd abc2
darcs pull -a ../bc
darcs changes

test `darcs changes | fgrep -c '* C'` -eq 1
