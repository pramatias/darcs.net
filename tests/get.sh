#!/usr/bin/env bash

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch t.t
darcs add t.t
darcs record -am "initial add"
darcs changes --context > my_context
DIR=`pwd`
abs_to_context="${DIR}/my_context"
cd ..
rm -rf temp2
darcs get temp1 --context="${abs_to_context}" temp2
darcs changes --context --repo temp2 > repo2_context
diff -u "${abs_to_context}" repo2_context
