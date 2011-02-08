set -x
cabal update
cabal install "$@" -fdeps-only 2>&1 > /dev/null | tee install-deps.log
grep -F -A 10000 "At least the following dependencies are missing" install-deps.log | \
grep -F -B 10000 "Error: some packages failed to install" | tail -n +2 | head -n -1 | \
while read -d, line || test -n "$line"; do
    cabal install "$@" "$line"
done
rm install-deps.log
