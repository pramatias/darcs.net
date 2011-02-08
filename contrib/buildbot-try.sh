#!/bin/bash
set -e

env | grep SSH_AGENT_PID
SSH_AGENT_CHECK_STATUS=$?
if [ $SSH_AGENT_CHECK_STATUS -ne 0 ]; then
    eval `ssh-agent`
      ssh-add
fi

echo "trying to obtain current password from darcs-unstable@darcs.net..."
password=`ssh darcs-unstable@darcs.net cat .buildbot-try.password`

if [ $SSH_AGENT_CHECK_STATUS -ne 0 ]; then
    ssh-agent -k
fi

test -n "$password" || {
    echo "that seems to have failed... :("
    exit 1
}

# Make a repo that's intersection of the upstream and ours and then pull our
# local patches on top. I wish there was a better (faster) way to do this.
echo "creating temporary repository in _buildbot.$$..."
rm -rf _buildbot.$$ && mkdir _buildbot.$$ && cd _buildbot.$$

darcs init
darcs pull --intersection .. http://darcs.net/ -a
darcs changes --context > _context
darcs tag --author "buildbot-try" "BUILDBOT TRY CONTEXT"
darcs pull -a .. --allow-conflicts # no markup please
count=`darcs changes --from-tag "BUILDBOT TRY CONTEXT" --count`

darcs diff --repo .. -u | patch -p1 # copy unrecorded changes
darcs diff --last $count -u > _diff # produce a nice diff

echo submitting build...
buildbot try --connect=pb --master buildmaster.darcs.net:9088 \
             --vc darcs -u darcs-review --passwd $password \
             --baserev "`cat _context`" \
             --diff _diff -p1 "$@"
