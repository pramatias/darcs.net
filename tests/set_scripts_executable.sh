#!/usr/bin/env bash

# Some tests for the --set-scripts-executable option.

. lib

abort_windows

rm -rf temp1 temp2

mkdir temp1
cd temp1
darcs init
cat > script.pl << FOO
#!/usr/bin/env perl
print "Hello\n";
FOO
chmod 0644 script.pl
date > nonscript
# pre-tests
test -r script.pl
test -r nonscript
test ! -x script.pl
test ! -x nonscript
darcs add script.pl nonscript
darcs record --name 'uno' --all
cd ..

# sans --set-scripts-executable (should not be executable)
mkdir temp2
cd temp2
darcs init
darcs pull -a ../temp1
# sanity check
test -r script.pl
test -r nonscript
# nothing should be executable
test ! -x script.pl
test ! -x nonscript
cd ..
rm -rf temp2

# with --set-scripts-executable
mkdir temp2
cd temp2
darcs init
darcs pull -a ../temp1 --set-scripts-executable
# sanity check
test -r script.pl
test -r nonscript
# script should be executable
test -x script.pl
test ! -x nonscript
cd ..
rm -rf temp2

# now let's try the same thing with get
darcs get --set-scripts-executable temp1 temp2
cd temp2
# sanity check
test -r script.pl
test -r nonscript
# script should be executable
test -x script.pl
test ! -x nonscript
cd ..

rm -rf temp1 temp2
