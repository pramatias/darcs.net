#!/usr/bin/env bash

set -ev

. lib

rm -rf temp1
mkdir temp1
cd temp1
darcs init
touch foo
darcs add foo

# Check that posthook works...
darcs whatsnew -s --posthook 'touch posthook-ran'
test -f posthook-ran
rm posthook-ran

# Check that posthook works with defaults...
echo ALL --posthook touch posthook-ran > _darcs/prefs/defaults
darcs whatsnew -s
test -f posthook-ran
rm posthook-ran

cd ..
rm -rf temp1

# POSIX-only section
# ----------------------------------------------------------------------
# Things below this section do not appear to work on Windows.
# Pending further investigation at http://bugs.darcs.net/issue1813

if echo $OS | grep -i windows; then
  exit 0
fi

# Check that DARCS_PATCHES_XML works
rm -rf R S                      # another script may have left a mess
darcs init      --repo R        # Create our test repos.
darcs init      --repo S        # Create our test repos.

cd R
echo 'echo $DARCS_PATCHES_XML' > hook
darcs record -lam 'hook'
chmod u+x hook
cat > _darcs/prefs/defaults << END
apply run-posthook
apply posthook ./hook
END
cd ..

cd S
echo 'Example content.' > f
darcs record -lam 'Add f'
darcs push -a ../R | grep 'patch author'
cd ..
