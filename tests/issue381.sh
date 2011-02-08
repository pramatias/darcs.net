#!/usr/bin/env bash
set -ev

# for issue381: "darcs send -o message --edit-description doesn't work"

DARCS_EDITOR=echo
export DARCS_EDITOR

rm -rf temp1 temp2
mkdir temp1 temp2

cd temp2
darcs init
cd ..

cd temp1
darcs init
echo Hello world > foobar
darcs add foobar
darcs record -a -A me -m add_foobar

# Test that editor is called when --output is used with --edit-description
echo This is a note > note
cat > editor <<EOF
#!/usr/bin/env bash
echo I am running the editor
echo the file is \$1
mv \$1 \$1-temp
echo hello world >> \$1
cat \$1-temp >> \$1
echo >> \$1
echo finished editing >> \$1
echo I am done running the editor
EOF

chmod +x editor

DARCS_EDITOR='bash editor' darcs send --author=me -a --output=bundle --edit-description ../temp2

echo === beginning of bundle > ===
cat bundle
echo === end of bundle > ===

grep ' add_foobar' bundle
grep 'finished editing' bundle

IFS=' ' darcs send --author=me -a --subject="it works" --to user@place.org --sendmail-command='grep "^Subject: it works$" %<' ../temp2

cd ..
rm -rf temp1 temp2
