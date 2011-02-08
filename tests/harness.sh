#!/usr/bin/env bash

set -vex

# Print some stuff out for debugging if something goes wrong:
echo $HOME
echo $PATH
which darcs
command -v darcs

# Check things that should be true when all the testscripts run

test -f "$HOME/lib"
password="AKARABNADABARAK-KARABADANKBARAKA"
grep $password "$HOME/test" || grep $password "$HOME/harness.sh"

if echo $OS | grep -i windows; then
    real=$(cmd //c echo $(command -v darcs.exe) | sed -e 's,\\,/,g')
    wanted=$(echo "$DARCS" | sed -e 's,\\,/,g')
    test "$real" = "$wanted"
else
    command -v darcs | fgrep "$DARCS"
fi
