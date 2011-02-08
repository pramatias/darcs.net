#!/bin/sh

set -ev

rm -rf temp1 temp2
mkdir temp1 temp2
cd temp1
darcs init
echo first > a
darcs add a
darcs record --pipe --all --name=first <<EOF
Thu Sep 18 22:37:06 MSD 2008
author
EOF

echo first > b
darcs add b
darcs record --pipe --all --name=first <<EOF
Thu Sep 18 22:37:06 MSD 2008
author
EOF

cd ../temp2
darcs init
darcs pull --all --dont-allow-conflicts ../temp1
echo second >> a
darcs record --pipe --all --name=second <<EOF
Thu Sep 18 22:37:07 MSD 2008
author
EOF

cd ../temp1
echo second >> b
darcs record --pipe --all --name=second <<EOF
Thu Sep 18 22:37:07 MSD 2008
author
EOF

darcs pull --all --dont-allow-conflicts ../temp2
test `darcs changes --count` = "4"
cd ..

rm -rf temp1 temp2
