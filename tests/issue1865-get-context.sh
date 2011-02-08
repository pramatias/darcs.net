#!/usr/bin/env bash

. lib

rm -rf temp1 temp2
mkdir temp1
cd temp1
darcs init
touch t.t
darcs add t.t
darcs record -am "initial add"
darcs tag -m tt
echo x > x
darcs rec -lam "x" x
darcs changes --context > my_context
abs_to_context="$(pwd)/my_context"
cd ..
darcs get temp1 --context="${abs_to_context}" temp2
darcs changes --context --repo temp2 > repo2_context
diff -u "${abs_to_context}" repo2_context
