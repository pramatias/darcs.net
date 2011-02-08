#!/usr/bin/env bash

# Some tests for 'darcs rec --edit-long-comment'

. lib

abort_windows

rm -rf temp1

export DARCS_EDITOR="/bin/cat -n"
# editor: space in command
mkdir temp1
cd temp1
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep '2.*END OF DESCRIPTION'
cd ..
rm -rf temp1

# editor: space in path
mkdir temp2\ dir
cd temp2\ dir
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep '2.*END OF DESCRIPTION'
cd ..
rm -rf temp2\ dir

# make sure summaries are coalesced
mkdir temp3
cd temp3
darcs init
cat > file <<EOF
1
2
3
4
EOF
darcs add file
darcs rec -a -m "init" file
cat > file <<EOF
A
2
3
B
EOF
echo y | darcs record --edit-long-comment -a -m edit | grep -c "./file" | grep 1
cd ..
rm -rf temp2\ dir

export DARCS_EDITOR='grep "END OF"'
# editor: quoting in command
mkdir temp1
cd temp1
darcs init
touch file.t
darcs add file.t
echo y | darcs record --edit-long-comment -a -m foo file.t | grep 'END OF'
cd ..
rm -rf temp1

export DARCS_EDITOR='/bin/echo'
# editor: evil filename
mkdir temp1
cd temp1
darcs init
touch file.t
darcs add file.t
touch '; test-command'
echo > test-command << FOO
#!/bin/sh
echo EVIL
FOO
chmod u+x test-command
echo y | darcs record --logfile='; test-command' --edit-long-comment -a -m foo file.t > log
not grep EVIL log
cd ..
rm -rf temp1
