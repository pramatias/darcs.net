
The following is provided as a reference for those interested in understanding
how the test suite works, and how to add and maintain tests.

Overview of types of tests
==========================

Darcs has tests in two formats.  Unit tests that directly test Haskell
functions are written in Haskell and live in modules under src/Darcs/Test.
Functional tests that test the darcs binary are written in Shell and live in
tests/.

Haskell tests
--------------------------

These are QuickCheck and HUnit tests primarily testing the Darcs core. The
Haskell modules containing these tests are under Darcs.Test in the module
hierarchy. They are called from src/unit.lhs using the test-framework package
from Hackage.

More about the Haskell tests can be found on http://wiki.darcs.net/Development/UnitTests.

Shell tests
---------------------------

Shell tests are useful because they are easy to create from a copy/paste
of actual shell commands executed. They are considered successful if no
bad exit codes are returned from the commands in the script. If the name of the
script file starts with 'failing-' however, the script is expected to return
a bad exit code. If such a script returns a bad exit code, this will not be
treated as a test suite failure. This is useful to document bugs and to-do
items.

How to run tests
=============================

To build the unit tests, pass the "-ftest" flag to "cabal configure" and then
do "cabal build". To run them, do "dist/build/unit/unit". They take a while.
Output like "[OK]" and "[OK, passed 100 tests]" is good. Output like
"Arguments exhausted after 33 tests" is a shortage of QuickCheck test
cases, not a test failure.

"runghc Setup.lhs test" causes all the functional tests in "tests/" to be run
against repositories in the old, hashed, and darcs2 formats.

Because "runghc Setup.lhs test" can take a long time to run, it's useful to run
fewer tests at once.  To help with that, 'runghc Setup.lhs test' accepts
arguments to run only a specific group of test scripts, or only named test
scripts:

  runghc Setup.lhs test tests       # all tests, excluding bugs or network tests
  runghc Setup.lhs test bugs        # all bugs (the 'failing-' scripts)
  runghc Setup.lhs test network     # the network tests

  # and this one runs tests/issue279_get_extra.sh and tests/repair_corrupt.sh
  runghc Setup.lhs test issue279_get_extra repair-corrupt

Tips for writing (and reading) tests
====================================

- Avoid including a repo format type to "darcs init"
  This insures that all three repo formats will be tested.
  However, if you know that the test only passes under some
  repo formats, *do* explicitly include a format option to "darcs init".


Tips for writing tests
----------------------

- Copy EXAMPLE.sh as a template to start from (don't forget to customise
  the headers!).

- Simply call darcs using "darcs" as you would in the shell.  It is the
  responsibility of the test harness to ensure that the darcs we are
  testing is first in the path.

- Always use Bash explicitly - this improves the portability of our tests.

- Always add this near the top of the script:

   set -ev

  The "v" causes the contents of the script to be printed as part of the run,
  which is helpful for debugging.  The "e" causes the script to exit as soon as
  there is an error.

- Try to avoid defining functions where possible.  This makes them
  harder to run and generally harder to use.  There are certainly cases
  where it is appropriate to define a function, but please do not do
  this just to avoid a little duplication.

- Also try to be careful using certain utilities; 'yes' is prohibited since
  it can cause infinite loops on Mac OS X; 'find' can be very useful, but
  options and behavior can differ from GNU find to the BSD finds to Solaris's
  find and so on. In general, stick to POSIX flags and functionality.

- There is a utility script intended for factoring out common calls and
  functions, called 'lib'. It can be invoked by adding a line like '. lib' to
  your shell script. lib provides 'set -ev', a common definition of 'not', and
  'abort_windows' for use in scripts which shouldn't run under Windows.
  You don't have to use lib if you don't want to, or if it causes problems.

- If you need to skip a test for any reason, the darcs-specific
  convention is to "exit 200".  This alerts the shell harness that the
  test was explicitly skipped and not passed.

- You can use the trap feature from bash to make ensure that darcs executes
  some command even if the test fails. Trapping ERR lets you have your
  last word just before a test fails. Trapping EXIT lets you do the
  same before any sort of explicit exit (such as the explicit 'exit 1'
  in the 'not' helper function).  For more details, see the bash man
  page or just grep trap in the test suite.
