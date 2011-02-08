#!/usr/bin/env bash
set -ev

DARCS_EDITOR=echo
export DARCS_EDITOR

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init

# setup test
cd ../temp1
darcs init
touch foo bar
darcs add foo bar
darcs record -a -m add_foo_bar -A x

# Test that a default preference value is not needed to send
darcs send --author=me -a --to=random@random --sendmail-command='grep "^To: random@random$" %<' ../temp2

# Test that a default preference will be used when no --to value is specified
echo "default@email" > ../temp2/_darcs/prefs/email
darcs send --author=me -a --sendmail-command='grep "^To: default@email$" %<' ../temp2

# Test that the --to parameter overrides the default value in the repository
darcs send --author=me -a --to=override@default --sendmail-command='grep "^To: override@default$" %<' ../temp2
darcs send --author=me -a --in-reply-to=some-thread-id --sendmail-command='grep "^In-Reply-To: some-thread-id$" %<' ../temp2
darcs send --author=me -a --in-reply-to=some-thread-id --sendmail-command='grep "^References: some-thread-id$" %<' ../temp2

# Test that the --subject parameter sets the subject

# Test that the --output-auto-name parameter outputs what we expect
darcs send --author=me -a --subject="it works" --output test1.dpatch ../temp2
darcs send --author=me -a --subject="it works" --output-auto-name ../temp2
cmp test1.dpatch add_foo_bar.dpatch

# test --output-auto-name works with optional argument.
mkdir patchdir
darcs send --author=me -a --subject="it works" --output-auto-name=patchdir ../temp2
cmp test1.dpatch patchdir/add_foo_bar.dpatch

# checking --output-auto-name=dir when run in different directory
cd patchdir
rm add_foo_bar.dpatch
darcs send --author=me -a --subject="it works" --output-auto-name=. ../../temp2
cmp ../test1.dpatch add_foo_bar.dpatch
cd ..

cd ..
rm -rf temp1 temp2
