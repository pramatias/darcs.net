#! /bin/bash

DIRNAME=`dirname "${0}"`
if [ "${DIRNAME:0:1}" = "/" ] ; then
 DARCSPACKAGEDIR="${DIRNAME}"
else
 DARCSPACKAGEDIR="${PWD}/${DIRNAME}"
fi

# If the DARCSPACKAGEDIR assignment above doesn't work for some funny reason, 
# you could set these variables by hand.  Or fix the script to work 
# automatically and submit a patch.

# Should be set to the full Cygwin path to the directory containing the 
# putty executables.
putty_binary_dir="${DARCSPACKAGEDIR}"
# Should be set to the full Cygwin path to the directory containing the 
# Windows binary "darcs.exe".
darcs_binary_dir="$putty_binary_dir"
# Should be set to the full Cygwin path to the Windows binary
# "darcs.exe".
darcs_binary="${darcs_binary_dir}/realdarcs.exe"

#---------------------------------------------------------------------
# Darcs Wrapper for Cygwin
#
# A Bash script that allows Cywin paths on the command line when using
# a version of Darcs compiled for Windows.  Darcs will still use still
# Windows paths internally.
#
#---------------------------------------------------------------------
# Usage
# 
# Edit this file and set the variables above.  Then, rename this 
# script to "darcs" and put it in your PATH somewhere before the 
# original binary.
#
# Darcs needs to launch itself for some operations and so the original
# binary needs to be in your Windows PATH.  Do not rename it.
#
#---------------------------------------------------------------------
# Known Issues
#
# This script is just a stopgap measure.  Things don't work perfectly.
# We really need a Cygwin build of Darcs.
#
# No path conversion is performed on:
#    - Any preferences set with "setpref"
#    - The "COMMAND" argument to "darcs trackdown"
#
# When Darcs launches external programs, it uses a Windows system call
# to do so.  This means you may not be able to run "hash bang" scripts
# directly.  For example, to run the Bash script "myscript", you'll
# have to tell Darcs to run "bash myscript".
#
# --------------------------------------------------------------------


PATH="$putty_binary_dir:$darcs_binary_dir:${PATH}"

debug=false

cmd="$1"

# Print each argument to stderr on a separate line.  Then exit.
function die() {
   local line
   for line in "$@"; do
      echo "$line" > /dev/stderr
   done
   exit 2
}

# Make sure 'darcs_binary_dir' is set.
if [ ! -d "$darcs_binary_dir" ]; then
   die "Please edit this script and set the 'darcs_binary_dir' variable" \
       "to refer to a valid directory." \
       "      script path = '$0'"  \
       "     darcs_binary_dir = '$darcs_binary_dir'"
fi

# Special case for when the first argument is an option.
if expr match "$cmd" '-' > /dev/null; then
   if $debug; then
      # echo "SIMPLE CASE:"
      for arg in "$@"; do
         echo "  arg = '$arg'"
      done
   else
      # echo about to exec -a darcs "$darcs_binary" "$@"
      exec -a darcs "$darcs_binary" "$@"
   fi
fi

# Shift off the darcs command name
shift

function is_opaque_opt() {
   local opt
   for opt in "${opaque_binary_opts[@]}"; do
      if [ "$opt" == "$1" ]; then
         return 0
      fi
   done
   return 1
}

function is_file_opt() {
   local opt
   for opt in "${file_binary_opts[@]}"; do
      if [ "$opt" == "$1" ]; then
         return 0
      fi
   done
   return 1
}

# Options are not dealt with in a command-specific way.  AFAIK, Darcs
# doesn't use the same option in two different ways, so we should be
# fine.

# List of "opaque" binary options.  These are options where we don't
# treat the option argument like a file.
declare -a opaque_binary_opts=( \
   '--repo-name' \
   '--to-match' '--to-patch' '--to-tag' \
   '--from-match' '--from-patch' '--from-tag' \
   '-t' '--tag' '--tags' '--tag-name' \
   '-p' '--patch' '--patches' \
   '-m' '--patch-name' \
   '--matches' '--match' \
   '--token-chars' \
   '-A' '--author' '--from' '--to' '--cc' \
   '--sign-as' '--creator-hash' \
   '--last' '--diff-opts' \
   '-d' '--dist-name' \
   '--log-file' \
   '--apply-as' \
   )

# List of binary options that take file arguments that need to be converted.
declare -a file_binary_opts=( \
   '--repodir' '--repo' '--sibling' \
   '--context' \
   '--logfile' '-o' '--output' \
   '--external-merge' \
   '--sign-ssl' '--verify' '--verify-ssl' \
   )

# --------------------------------------------------------------------
# The three command categories.  We only use the first one, but the
# others are listed to make sure we've covered everything.  Luckily,
# there aren't any commands that have some args that need to be
# converted and some that don't.

# Commands whose arguments are file paths that need to be translated.
cmds_convert_nonoption_args='|get|put|pull|push|send|apply'

# Commands who's arguments should be left alone.  File paths that
# refer to files in the repo should NOT be converted because they
# are relative paths, which Darcs will handle just fine.  Cygwin
# sometimes makes them absolute paths, which confuses Darcs.
#cmds_no_convert_nonoption_paths='|add|remove|mv|replace|record|whatsnew|changes|setpref|trackdown|amend-record|revert|diff|annotate'

# Commands that don't accept non-option arguments
#cmds_no_nonoption_args='|initialize|tag|check|optimize|rollback|unrecord|unpull|resolve|dist|repair'

# See if we need to convert the non-option args for the current
# command.  This matches some prefix of one of the commands in the
# list.  The match may not be unambiguous, we can rely on Darcs to
# deal with that correctly.
if expr match "$cmds_convert_nonoption_args" ".*|$cmd" > /dev/null; then
   convert_nonoption_args=true
else
   convert_nonoption_args=false
fi

function convert_path() {
   # echo "converting path ${*} ..." >> /tmp/log
   if expr match "$1" '[-@._A-Za-z0-9]*:' > /dev/null; then
      # Some sort of URL or remote ssh pathname ("xxx:/")
      echo "$1"
      # echo "converting path ${*} ... to ${1}" >> /tmp/log
   elif [ "$1" == '.' ]; then
      # Compensate for stupid 'cygpath' behavior.
      echo '.'
      # echo "converting path ${*} ... to ." >> /tmp/log
   else
      cygpath -wl -- "$1"
      # echo "converting path ${*} ... to `cygpath -wl -- ${1}`" >> /tmp/log
   fi
}

declare -a params=("$cmd")

num_nonoption_args=0

while [ $# -gt 0 ]; do
   arg=$1
   shift
   if expr match "$arg" '-' > /dev/null; then
      # It's an option.  Check to see if it's an opaque binary option.

      if expr match "$arg" '.*=' > /dev/null; then
         # The option has an '=' in it.
         opt=`expr match "$arg" '\([^=]*\)'`
         opt_arg=`expr match "$arg" '[^=]*=\(.*\)'`
         if is_opaque_opt "$opt"; then
            true;
         elif is_file_opt "$opt"; then
            opt_arg=`convert_path "$opt_arg"`
         else
            die "darcs-wrapper: I don't think '$opt' accepts an argument." \
                "[ If it does, then there is a bug in the wrapper script. ]"
         fi
         params[${#params[*]}]="$opt=$opt_arg"

      else
         # The option doesn't have an '='
         opt="$arg"
         if is_opaque_opt "$opt"; then
            if [ $# -eq 0 ]; then
               die "darcs-wrapper: I think '$arg' requires an argument." \
                   "[ If it doesn't, then there is a bug in the wrapper script. ]"
            fi
            opt_arg="$1"
            shift
            params[${#params[*]}]="$opt"
            params[${#params[*]}]="$opt_arg"
         elif is_file_opt "$opt"; then
            if [ $# -eq 0 ]; then
               die "darcs-wrapper: I think '$arg' requires an argument." \
                   "[ If it doesn't, then there is a bug in the wrapper script. ]"
            fi
            opt_arg=`convert_path "$1"`
            shift
            params[${#params[*]}]="$opt"
            params[${#params[*]}]="$opt_arg"
         else
            params[${#params[*]}]="$opt"
         fi
      fi

   else
      if $convert_nonoption_args; then
         arg=`convert_path "$arg"`
      fi
      params[${#params[*]}]="$arg"
      (( num_nonoption_args += 1 ))
   fi
done

# DEBUG
if $debug; then
   echo "ARGS:"
   for arg in "${params[@]}"; do
      echo "  arg = '$arg'"
   done
else
   # echo about to exec -a darcs "$darcs_binary" "${params[@]}"
   exec -a darcs "$darcs_binary" "${params[@]}"
fi

