#!/usr/bin/env bash
## Test for issue64 - Should store patch metadata in UTF-8
##
## Copyright (C) 2009  Reinier Lamers
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

. lib 

# Helper function: do a darcs changes --xml and grep the result for the first
# argument. If it is not found, exit with status 1. Otherwise, continue. The
# second argument is a text that describes what we're grepping for.
# If a third argument is given, it is used as the value for a --last option for
# darcs changes.
grep_changes () {
    if [ -z "$3" ]; then
            last=""
    else
            last="--last $3"
    fi

    darcs changes $last --xml > changes.xml
    if grep "$1" changes.xml ; then
        echo "$2 OK"
    else
        echo "$2 not UTF-8-encoded!"
        exit 1
    fi
}


# This file is encoded in ISO-8859-15 aka latin9. It was crafted with a hex editor.
# Please leave it this way :-)
switch_to_latin9_locale

rm -rf temp1
mkdir temp1
cd temp1

darcs init

# Test recording non-UTF-8-encoded non-latin1 ("funny") metadata from
# interactive input

echo 'Selbstverst‰ndlich ¸berraschend' > something.txt
darcs add something.txt

echo 'l33tking∏0r@example.org' > interaction_script.txt
echo y >> interaction_script.txt
echo y >> interaction_script.txt
echo '§uroh4xx0rz' >> interaction_script.txt
echo n >> interaction_script.txt

unset DARCSEMAIL
unset DARCS_TESTING_PREFS_DIR
unset EMAIL
set
darcs record -i < interaction_script.txt
grep_changes 'l33tking≈æ0r@example.org' 'patch author from interactive prompt'
grep_changes '‚Ç¨uroh4xx0rz' 'patch name from interactive prompt'

# Test recording funny metadata from command line 

echo 'Sogar ¸berraschender' >> something.txt

darcs record -a -A 'JÈrÙme LebΩuf' -m 'that will be § 15, sir'

grep_changes 'that will be ‚Ç¨ 15, sir' 'patch name from command line'
grep_changes 'J√©r√¥me Leb≈ìuf' 'patch author from command line'

# Test recording funny metadata from a log file

echo 'Am aller¸berraschendsten' >> something.txt

echo 'darcs is soms wat naÔef aangaande tekstcodering' > log.txt
echo 'en zulke naÔviteit is tegenwoordig passÈ, aldus ¥i∏ek' >> log.txt
darcs record -a -A 'JÈrÙme LebΩuf' --logfile=log.txt

grep_changes 'darcs is soms wat na√Øef aangaande tekstcodering' 'patch name from log file'
grep_changes 'en zulke na√Øviteit is tegenwoordig pass√©, aldus ≈Ωi≈æek' 'patch log from log file'

# Test recording funny metadata from environment, 
export EMAIL='Slavoj ¥i∏ek <zizek@example.edu>'
rm _darcs/prefs/author
echo 'La la la, the more lines the better!' >> something.txt
darcs record -a -m 'Patch names are overrated'

grep_changes 'Slavoj ≈Ωi≈æek' 'author name from environment'

# Test recording funny metadata from prefs files
echo '¥ed is dead' > _darcs/prefs/author
echo '483 bottles of beer on the wall' >> something.txt
darcs record -a -m 'Patch by ¥ed'

grep_changes '≈Ωed is dead' 'author name from prefs file'

# Test amend-recording funny metadata
echo 'No, it is really 484' >> something.txt
echo y | darcs amend-record -p 'Patch by ' -A '¥ed is even deader' -a
grep_changes '≈Ωed is even deader' 'author name from amend-record command line flag'

echo '#!/usr/bin/env bash' > editor
echo 'echo All my §s are gone > $1' >> editor # create an 'editor' that writes latin9
chmod +x editor
export EDITOR="`pwd`/editor"
printf "y\ny\n" | darcs amend --edit -p 'Patch by '
grep_changes 'All my ‚Ç¨s are gone' 'description edited from amend-record'
grep_changes '≈Ωed is even deader' 'author name taken from draft in amend'

# Test rollback recording funny metadata
printf "y\ny\n" | darcs rollback -p 's are gone' -A '¥ee¥ee' -m "No patch§s by ¥ed!"
grep_changes '≈Ωee≈Ωee' 'Author name from rollback command line'
grep_changes 'No patch‚Ç¨s by ≈Ωed' 'Patch name from rollback command line'
grep_changes 'All my ‚Ç¨s are gone' 'Patch name of rolled back patch' 1
grep_changes '≈Ωed is even deader' 'Author name of rolled back patch' 1

# Test tag recording funny metadata
rm _darcs/prefs/author # Make tag be taken from EMAIL env variable
darcs tag -m '¥ is my favorite letter'
grep_changes 'Slavoj ≈Ωi≈æek' 'author name from environment with tag command' 1
grep_changes '≈Ω is my favorite letter' 'Tag name from command line'

unset EMAIL
printf "¥ors\ninitialcomment\n" | darcs tag --edit-long-comment
grep_changes ≈Ωors 'Author name from interactive prompt from tag command'
grep_changes 'All my ‚Ç¨s are gone' 'Tag name from editor from tag command' 1

if grep ¥ors _darcs/prefs/author ; then
    echo 'Author name stored locale-encoded in prefs file after tag command, OK'
else
    echo 'No locale-encoded author in prefs file after tag command!'
    exit 1
fi

darcs tag -A Ad∏e -m 'La∏t call'
grep_changes Ad≈æe 'Author name from tag command line' 1
grep_changes 'La≈æt call' 'Tag name from tag command line (take 2)' 1

cd ..

# test that UTF-8 metadata doesn't get mangled on get
rm -rf temp2
darcs get temp1 temp2
darcs changes --repodir temp1 --xml > temp1/changes.xml
darcs changes --repodir temp2 --xml > temp2/changes.xml
diff temp1/changes.xml temp2/changes.xml

# and that it doesn't get mangled on push
rm -rf temp2
mkdir temp2; darcs init --repodir temp2
darcs push --repodir temp1 -a temp2 --set-default
darcs changes --repodir temp1 --xml > temp1/changes.xml
darcs changes --repodir temp2 --xml > temp2/changes.xml
diff temp1/changes.xml temp2/changes.xml

# and that it doesn't get mangled on pull
rm -rf temp2
mkdir temp2; darcs init --repodir temp2
darcs pull --repodir temp2 -a temp1
darcs changes --repodir temp1 --xml > temp1/changes.xml
darcs changes --repodir temp2 --xml > temp2/changes.xml
diff temp1/changes.xml temp2/changes.xml

# and that it doesn't get mangled on send
rm -rf temp2
mkdir temp2; darcs init --repodir temp2
darcs send --repodir temp1 -a -o temp2/patch.dpatch
darcs apply --repodir temp2 -a temp2/patch.dpatch
darcs changes --repodir temp1 --xml > temp1/changes.xml
darcs changes --repodir temp2 --xml > temp2/changes.xml
diff temp1/changes.xml temp2/changes.xml
