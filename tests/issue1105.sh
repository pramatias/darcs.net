#!/usr/bin/env bash

. lib

rm -rf temp
mkdir temp
cd temp
darcs init
darcs changes

echo changes summary > _darcs/prefs/defaults
darcs changes
echo changes summary arg > _darcs/prefs/defaults
not darcs changes
echo ALL summary > _darcs/prefs/defaults
darcs changes
echo ALL summary arg > _darcs/prefs/defaults
not darcs changes

echo changes last 10 > _darcs/prefs/defaults
darcs changes
echo changes last > _darcs/prefs/defaults
not darcs changes
echo ALL last 10 > _darcs/prefs/defaults
darcs changes
echo ALL last > _darcs/prefs/defaults
not darcs changes

echo changes author me > _darcs/prefs/defaults
not darcs changes
echo ALL author me > _darcs/prefs/defaults
darcs changes
echo ALL unknown > _darcs/prefs/defaults
not darcs changes

cd ..
rm -rf temp
