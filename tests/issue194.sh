set -ev

rm -rf temp1 temp2

mkdir temp1; cd temp1 ; darcs init ; cd ..
darcs get temp1 temp2
cd temp2/ ; echo 'x' > _darcs/prefs/author ; cd ..
cd temp1/ ; echo 'x' > _darcs/prefs/author ; cd ..

cd temp1/
touch test ; darcs record
darcs add test     ; darcs record -a -m 'test'
darcs mv test best ; darcs record -a -m 'test -> best'
darcs mv best test ; darcs record -a -m 'best -> test'
cd ..

cd temp2/
touch test2
darcs add test2     ; darcs record -a -m 'test2'
darcs mv test2 best ; darcs record -a -m 'test2 -> best'
darcs mv best test2 ; darcs record -a -m 'best -> test2'

darcs pull ../temp1/ -a
cd ..

rm -rf temp1 temp2
