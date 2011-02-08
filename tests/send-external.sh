#!/usr/bin/env bash
set -ev

DARCS_EDITOR=echo
export DARCS_EDITOR

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init
cd ..

cd temp1
darcs init
date > foobar
darcs add foobar
darcs rec -a -m add-foobar

cat > saveit.sh <<EOF
#!/bin/sh
# send-mail1.sh: Test sendmail command for darcs send 1
# 2008-Oct-06 22.25 / TN
set -ev
echo all: \$0 "\$@" >>saved.out
echo \$6 contains: >>saved.out
ls -ltr >>saved.out
cat "\$6" >>saved.out
echo End of \$6 contents >>saved.out
grep add-foobar \$6
CNT=0
while [ "\$#" != "0" ]; do
  CNT=`expr \$CNT + 1`
  echo \$0: arg[\$CNT] = \"\$1\" >>saved.out
  shift
done
echo \$0: Total \$CNT arguments >>saved.out
echo \$0: Input: >>saved.out
cat >>saved.out
echo \$0: End of input: >>saved.out
EOF

chmod +x saveit.sh

# foobar
darcs send --author=me -a --to=random@random \
    --sendmail-command='bash ./saveit.sh %s %t %c %b %f %a %S %t %C %B %F %A something' ../temp2

cat saved.out
grep add-foobar saved.out
grep 'addfile ./foobar' saved.out

cd ..
rm -rf temp1 temp2
