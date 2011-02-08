#!/usr/bin/env bash
. lib

rm -rf temp && mkdir temp
cd temp
darcs init
echo -n > no_newline
echo -n > newline

darcs rec -lam "empty"

echo -n foo > no_newline
echo foo > newline

wc -l no_newline | grep 0
wc -l newline | grep 1

darcs wh > diff1
darcs rec -am "add bits"
darcs revert -a | grep "no changes"

echo -n > no_newline
echo -n > newline

darcs wh > diff2
darcs rec -lam "bar"

darcs revert -a | grep "no changes"
darcs check

cat > diff1.expected <<EOF
hunk ./newline 1
+foo
hunk ./no_newline 1
-
+foo
EOF

cat > diff2.expected <<EOF
hunk ./newline 1
-foo
hunk ./no_newline 1
-foo
+
EOF

diff -u diff1.expected diff1
diff -u diff2.expected diff2

cd .. && rm -rf temp
