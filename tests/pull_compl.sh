#!/usr/bin/env bash

## Public domain 2007  Kevin Quick
##
## This file is included as part of the Darcs test distribution,
## which is licensed to you under the following terms:
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


set -ev

rm -rf temp1 temp2 temp3 temp4 temp5

mkdir temp1
cd temp1
cat > foo <<EOF
line1
line2
line3
line4
line5
line6
line7
EOF
darcs initialize
darcs add foo
darcs record -a -m addfoo

cd ..
darcs get temp1 temp4

chgrec () {
    set -ev
    sed -e "$1" foo > foo.tmp
    mv foo.tmp foo
    darcs record -a --ignore-times -m "$2"
}

cd temp1
chgrec 's/line2/line2\nline2.1\nline2.2/' inssub2
chgrec 's/line4/Line 4/' Line4

darcs changes | grep ' \*'
echo done with changes on temp1 > /dev/null

cd ..
darcs get temp1 temp2
darcs get temp1 temp3
cd temp1

chgrec 's/line1/line0\nline1/' line0
chgrec 's/Line 4/LINE FOUR/' LINE4
chgrec 's/line7/line7\nLastLine/' LastLine
chgrec 's/LINE FOUR/LINE FOUR\nline4.1/' line4.1

darcs changes | grep ' \*'
echo done with changes on temp1 > /dev/null

cd ../temp3
darcs pull -p LastLine -av
chgrec 's/line1$/FirstLine/' FirstLine

cd ../temp4

darcs changes | grep ' \*'
echo done with changes on temp4 > /dev/null

darcs pull ../temp1 --dry-run | grep ' \*'
darcs pull ../temp1 --dry-run | grep ' \*' > p1.out
cat > p1.req <<EOF
  * inssub2
  * Line4
  * line0
  * LINE4
  * LastLine
  * line4.1
EOF
diff p1.req p1.out

darcs pull ../temp1 --dry-run --complement | grep ' \*' > p2.out
diff p1.out p2.out

darcs pull --dry-run --complement ../temp1 ../temp2 | grep ' \*' > p3.out
cat > p3.req <<EOF
  * line0
  * LastLine
EOF
diff p3.req p3.out

darcs pull --dry-run --complement ../temp1 ../temp3 | grep ' \*' > p4.out
cat > p4.req <<EOF
  * line0
EOF
diff p3.req p3.out

darcs pull --dry-run --complement ../temp1 ../temp2 ../temp3 | grep ' \*' > p5.out
diff p4.out p5.out

darcs pull --dry-run --complement ../temp1 ../temp2 ../temp3 ../temp2 ../temp2 ../temp3 ../temp3 ../temp2 | grep ' \*' > p6.out
diff p4.out p6.out

darcs pull --dry-run --complement ../temp3 ../temp2 | grep ' \*' > p7.out
cat > p7.req <<EOF
  * LastLine
EOF

darcs pull --dry-run --complement ../temp2 ../temp3 > p8.out
grep "No remote changes to pull in!" p8.out

# because duplicates are stripped before performing action,
# this is the same as: darcs pull ../temp1
darcs pull --dry-run --complement ../temp1 ../temp1 > fooout
cat fooout
grep ' \*' fooout > p9.out
diff p1.req p9.out

# so the "null" pull must be tested this way:
darcs get ../temp1 ../temp5
darcs pull --dry-run --complement ../temp1 ../temp5 > p9.out
grep "No remote changes to pull in!" p9.out

darcs pull -av --complement ../temp1 ../temp3
darcs check

cd ..
rm -rf temp1 temp2 temp3 temp4 temp5
