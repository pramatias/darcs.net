#!/usr/bin/env bash
set -ev

if [ x${REMOTE_DIR} = x ]; then
  REMOTE_DIR=sshtest
fi

if [ x"${USE_PUTTY}" != x ]; then
  DARCS_SSH=plink
  export DARCS_SSH
  DARCS_SCP=pscp
  export DARCS_SCP
  DARCS_SFTP=psftp
  export DARCS_SFTP
fi

if [ x"${USE_CONTROL_MASTER}" != x ]; then
  DARCS_SSH_FLAGS="--ssh-cm"
  export DARCS_SSH_FLAGS
fi

if [ x"${DARCS_SSH}" = x ]; then
  SSH=ssh
else
  SSH=${DARCS_SSH}
fi

rm -rf tempssh
mkdir tempssh
cd tempssh

cleanup () {
  cd ..
  rm -rf tempssh
}

if [ x${REMOTE} = x ]; then
  echo
  echo "Note: to enable full SSH testing, set REMOTE to some SSH path first,"
  echo "      e.g. REMOTE=you@server.org $0"
  cleanup
  exit 200
fi

# ================ Setting up remote repositories ===============
${SSH} ${REMOTE} "\
rm -rf ${REMOTE_DIR}; \
mkdir ${REMOTE_DIR}; \
cd ${REMOTE_DIR}; \
\
mkdir testrepo; cd testrepo; \
darcs init; \
echo moi > _darcs/prefs/author; \
touch a; darcs add a; darcs record a --ignore-times -am 'add file a'; \
echo 'first line' > a; darcs record a --ignore-times -am 'add first line to a'; \
cd ..; \
\
darcs get testrepo testrepo-pull; \
cd testrepo-pull; \
echo moi > _darcs/prefs/author; \
touch b; darcs add b; darcs record b --ignore-times -am 'add file b'; \
echo 'other line' > b; darcs record b --ignore-times -am 'add other line to b'; \
cd ..; \
\
darcs get testrepo testrepo-push; \
darcs get testrepo testrepo-send; \
"

# ================ Settings ===============
echo ${DARCS_SSH_FLAGS}
echo ${DARCS_SSH}
echo ${DARCS_SCP}
echo ${DARCS_SFTP}

# ================ Checking darcs get ==================
${DARCS} get ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo ${DARCS_SSH_FLAGS}
# check that the test repo made it over
[ -d testrepo ]
[ -d testrepo/_darcs ]
[ -f testrepo/a ]

# if the above test is disabled we just init a blank repo
# so that the other tests can continue
if [ ! -d testrepo ]; then
  mkdir testrepo
  cd testrepo
  darcs init
  cd ..
fi

# ================ Checking darcs pull =================
${DARCS} get ${DARCS_SSH_FLAGS} testrepo testrepo-pull
cd testrepo-pull
echo yy | ${DARCS} pull ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-pull
# see if the changes got pulled over
grep "other line" b

cd ..

# ================ Checking darcs push and send ================="
${DARCS} get ${DARCS_SSH_FLAGS} testrepo testrepo-push
cd testrepo-push
echo moi > _darcs/prefs/author
echo "second line" >> a; ${DARCS} record a --ignore-times -am "add second line to a"
touch c; ${DARCS} add c
${DARCS} record --ignore-times -am "add file c" c
echo yy | ${DARCS} push ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-push
# check that the file c got pushed over
${SSH} ${REMOTE} "[ -f ${REMOTE_DIR}/testrepo-push/c ]"
echo yy | ${DARCS} send ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-send -o mybundle.dpatch
# check that the bundle was created
grep "add file c" mybundle.dpatch
cd ..

# ================ Checking darcs put =================="
cd testrepo
${DARCS} put ${DARCS_SSH_FLAGS} ${REMOTE}:${REMOTE_DIR}/testrepo-put
# check that the put was successful
${SSH} ${REMOTE} "[ -d ${REMOTE_DIR}/testrepo-put/_darcs ]"
${SSH} ${REMOTE} "[ -f ${REMOTE_DIR}/testrepo-put/a ]"
cd ..

# ======== Checking push over ssh with a conflict ========="
${SSH} ${REMOTE} "echo apply no-allow-conflicts >> ${REMOTE_DIR}/testrepo-put/_darcs/prefs/defaults"

cd testrepo 
echo 'change for remote' > a
darcs record --ignore-times -am 'change for remote'
darcs push -a
darcs ob --last 1 -a
echo 'change for local' > a
darcs record --ignore-times -am 'change for local'

if darcs push -a 2>&1 | grep -q 'conflicts options to apply' ; then
    # do nothing. 
    echo "OK";
else
    exit 1;
fi

cd ..

cleanup
