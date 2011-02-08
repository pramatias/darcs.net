#!/usr/bin/env bash

. lib

set -ev
switch_to_latin9_locale
rm -rf temp1
rm -rf temp2
rm -rf temp3
mkdir temp1
mkdir temp2
mkdir temp3
cd temp1

seventysevenaddy="<aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa@bbbbbbbbbb.cccccccccc.abrasoft.com>"

darcs init

echo "Have you seen the smørrebrød of René Çavésant?" > non_ascii_file
darcs add non_ascii_file
darcs record -am "non-ascii file add" -A test

cd ../temp2
darcs init
cd ../temp1

# long email adress: check that email adresses of <= 77 chars don't get split up
darcs send --from="Kjålt Überström $seventysevenaddy" \
           --subject "Un patch pour le répositoire" \
           --to="Un garçon français <garcon@francais.fr>" \
           --sendmail-command='tee mail_as_file %<' \
           -a ../temp2

cat mail_as_file
# The long mail address should be in there as a whole
grep $seventysevenaddy mail_as_file

# Check that there are no non-ASCII characters in the mail
cd ../temp3
cat > is_ascii.hs <<EOF
import Data.Char (chr)

main = getContents >>= print . not . any (> Data.Char.chr 127)
EOF

ghc --make is_ascii.hs -o is_ascii
./is_ascii < ../temp1/mail_as_file | grep '^True$'

cd ..
rm -rf temp1
rm -rf temp2
rm -rf temp3
