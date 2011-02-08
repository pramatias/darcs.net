#!/usr/bin/env bash

# Testing amend-record.

. lib

rm -rf temp1
# set up the repository
mkdir temp1
cd temp1
darcs init
cd ..

# do some work here
cd temp1
# Plain amend-record
touch foo
darcs add foo
darcs record -a -m add_foo
echo 'another line' > foo
echo y | darcs amend -a foo | grep -i 'amending changes'
darcs changes -v | grep 'another line'
# amend-record of removed file
touch bar1
touch bar2
cat > bar1 << FOO
a line
b line
FOO
darcs add bar1 bar2
darcs record -a -m add_bars
rm -f bar2
echo y | darcs amend -a | grep -i 'finished amending'
# Special case: patch is empty after amend
cp foo foo.old
echo 'another line' >> foo
darcs record -a -m add_line foo | grep -i 'finished recording'
mv foo.old foo
echo y | darcs amend -a foo | grep -i 'amending changes'
# Amend --author, -m, etc
echo "another line" >> foo
echo y | darcs amend -a -m new_name foo | grep -i 'amending changes'
darcs changes --last=1 | grep new_name
echo "another line" >> foo
echo y | darcs amend -a -m new_name -A new_author foo | grep -i 'amending changes'
darcs changes --last=1 | grep new_author

# check that normally the date changes when we amend
echo "another line" >> foo
darcs changes --last=1 | head -n 1 > old_date
sleep 1
echo y | darcs amend -a foo -A new_author | grep -i 'amending changes'
darcs changes --last=1 | head -n 1 > new_date
not cmp old_date new_date

# check that --keep-date works
echo "another line" >> foo
darcs changes --last=1 | head -n 1 > old_date
sleep 1
echo y | darcs amend -a foo -A new_author --keep-date | grep -i 'amending changes'
darcs changes --last=1 | head -n 1 > new_date
cmp old_date new_date

cd ..

# check that the identity changes with --keep-date
darcs get temp1 temp2
cd temp2

echo "another line" >> foo
darcs changes --last=1 | head -n 1 > old_date
echo y | darcs amend -a foo -A new_author --keep-date | grep -i 'amending changes'
darcs pull ../temp1 -a --skip-conflicts | grep -i "Skipping some"

cd ..


rm -rf temp1 temp2
