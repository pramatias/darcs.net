#!/bin/bash

set -ev

range="0 1 2"

rm -rf temp1 temp2              # clean up when previous run crashed
mkdir temp1 && cd temp1 && darcs init
touch f && darcs add f
for i in $range
do
  echo $i > f && darcs record -A me --ignore-time -m p$i --all f
done

cd .. && mkdir temp2 && cd temp2 && darcs init

set -x

: Demonstrate problem with regrem
for i in $range
do
  darcs pull --patch p$i ../temp1 --all
  if darcs whatsnew ; then
      : Resolve conflict - rollback our patch
      darcs revert --all
      darcs rollback -a --name x0
      darcs revert --all
  fi
  : Create local change and record it
  echo X$i > f && darcs record -A me --ignore-time -m x$i --all f
done

cd ..
rm -rf temp1 temp2
