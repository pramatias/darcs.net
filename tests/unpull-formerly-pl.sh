#!/usr/bin/env bash
set -ev

rm -rf tempA
mkdir tempA
cd tempA
darcs initialize
echo hello world > foo
darcs add foo
darcs record -a -m hellofoo

echo goodbye world >> foo
darcs record -a -m goodbyefoo

darcs replace world bar foo
echo Hi there foo > bar
darcs add bar
darcs record -a -m addbar

darcs mv bar baz
darcs replace bar baz foo
darcs record -a -m bar2baz

echo Do not love the baz, or anything in the baz. >> foo
darcs record -a -m nolove

darcs mv baz world
darcs replace baz world foo
darcs record -a -m baz2world

darcs whatsnew | grep 'No changes'

grep 'love the world' foo

echo y | darcs unpull -p baz2world

darcs whatsnew | grep 'No changes'

grep 'love the baz' foo

echo y | darcs unpull -p bar2baz

grep 'love the bar' foo

echo y | darcs unpull -p nolove

grep 'love' foo && exit 1 || true

cd ..
rm -rf tempA

