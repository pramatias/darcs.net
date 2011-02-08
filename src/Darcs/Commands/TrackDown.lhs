%  Copyright (C) 2002-2005 David Roundy
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published by
%  the Free Software Foundation; either version 2, or (at your option)
%  any later version.
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; see the file COPYING.  If not, write to
%  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
%  Boston, MA 02110-1301, USA.

\darcsCommand{trackdown}
\begin{code}
module Darcs.Commands.TrackDown ( trackdown ) where
import Prelude hiding ( init )
import System.Exit ( ExitCode(..) )
import System.Cmd ( system )
import System.IO ( hFlush, stdout )
import Control.Monad( when )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag(SetScriptsExecutable, Bisect), workingRepoDir, bisect,
                         setScriptsExecutableOption, makeScriptsExecutable )
import Darcs.Patch.PatchInfoAnd ( hopefully )
import Darcs.Repository ( amInRepository, readRepo, withRepoReadLock, RepoJob(..), withRecorded,
                          setScriptsExecutable )
import Darcs.Witnesses.Ordered ( RL(..), (:<)(..), (+<+),
                                 reverseRL, splitAtRL, lengthRL, mapRL, mapFL, mapRL_RL )
import Darcs.Patch.Conflict ( Conflict )
import Darcs.Patch.FileHunk ( IsHunk )
import Darcs.Patch.Format ( PatchListFormat )
import Darcs.Patch.Patchy ( Patchy, Invert, Apply, ShowPatch )
import Darcs.Patch ( RepoPatch, Named, description, apply, invert )
import Darcs.Patch.Set ( newset2RL )
import Printer ( putDocLn )
import Darcs.Test ( getTest )
import Darcs.Lock ( withTempDir )

#include "gadts.h"

trackdownDescription :: String
trackdownDescription = "Locate the most recent version lacking an error."

trackdownHelp :: String
trackdownHelp =
 "Trackdown tries to find the most recent version in the repository which\n"++
 "passes a test.  Given no arguments, it uses the default repository test.\n"++
 "Given one argument, it treats it as a test command.  Given two arguments,\n"++
 "the first is an initialization command with is run only once, and the\n"++
 "second is the test command.\n\n"++
 "Without the --bisect option, trackdown does linear search starting from head,\n"++
 "and moving away from head.  With the --bisect option, it does binary search.\n\n"++
 "Under the assumption that failure is monotonous, trackdown produces\n"++
 "the same result with and without --bisect.  (Monotonous means that when\n"++
 "moving away from head, the test result changes only once from \"fail\" to \"ok\".)\n"++
 "If failure is not monotonous, any one of the patches that break the test is\n"++
 "found at random."

trackdown :: DarcsCommand
trackdown = DarcsCommand {commandProgramName = "darcs",
                          commandName = "trackdown",
                          commandHelp = trackdownHelp,
                          commandDescription = trackdownDescription,
                          commandExtraArgs = -1,
                          commandExtraArgHelp = ["[[INITIALIZATION]",
                                                    "COMMAND]"],
                          commandCommand = trackdownCmd,
                          commandPrereq = amInRepository,
                          commandGetArgPossibilities = return [],
                          commandArgdefaults = nodefaults,
                          commandAdvancedOptions = [setScriptsExecutableOption],
                          commandBasicOptions = [workingRepoDir, bisect]}

trackdownCmd :: [DarcsFlag] -> [String] -> IO ()
trackdownCmd opts args = withRepoReadLock opts $ RepoJob $ \repository -> do
  patches <- readRepo repository
  (init,test) <- case args of
          [] ->
              do t <- getTest opts
                 return (return ExitSuccess, t)
          [cmd] ->
              do putStrLn $ "Tracking down command:\n"++cmd
                 return $ (return ExitSuccess, system cmd)
          [init,cmd] ->
              do putStrLn $ "Initializing with command:\n"++init
                 putStrLn $ "Tracking down command:\n"++cmd
                 return $ (system init, system cmd)
          _ -> fail "Trackdown expects zero to two arguments."
  withRecorded repository (withTempDir "trackingdown") $ \_ -> do
    when (SetScriptsExecutable `elem` opts) setScriptsExecutable
    _ <- init
    (if Bisect `elem` opts
     then trackBisect
     else trackNextLinear) opts test (mapRL_RL hopefully . newset2RL $ patches)

-- | linear search (without --bisect)
trackNextLinear :: RepoPatch p => [DarcsFlag] -> IO ExitCode -> RL (Named p) C(x y) -> IO ()
trackNextLinear opts test (p:<:ps) = do
    test_result <- test
    if test_result == ExitSuccess
       then putStrLn "Success!"
       else do apply (invert p) `catch` \e -> fail ("Bad patch:\n" ++ show e)
               makeScriptsExecutable opts (invert p)
               putStrLn "Trying without the patch:"
               putDocLn $ description $ invert p
               hFlush stdout
               trackNextLinear opts test ps
trackNextLinear _opts test NilRL = do
    test_result <- test
    if test_result == ExitSuccess
       then putStrLn "Success!"
       else putStrLn "Noone passed the test!"

-- | binary search (with --bisect)
trackBisect :: (IsHunk p, Conflict p, PatchListFormat p, Patchy p) => [DarcsFlag] -> IO ExitCode -> RL p C(x y) -> IO ()
trackBisect _ test NilRL = do
    test_result <- test
    if test_result == ExitSuccess
       then putStrLn "Success!"
       else putStrLn "Noone passed the test!"
trackBisect opts test ps = do
      test_result <- test
      if test_result == ExitSuccess
        then putStrLn ("Test does not fail on head.")
        else trackNextBisect opts curr_prog test BisectRight (patchTreeFromRL ps)
    where
      curr_prog = (1, 1 + round ((logBase 2 $ fromIntegral $ lengthRL ps) :: Double)) :: (Int,Int)

-- | Bisect Patch Tree
data PatchTree p C(x y) where
    Leaf :: p C(x y) -> PatchTree p C(x y)
    Fork :: PatchTree p C(y z) -> PatchTree p C(x y) -> PatchTree p C(x z)

-- | Direction of Bisect trackdown
data BisectDir = BisectLeft | BisectRight deriving Show

-- | Progress of Bisect
type BisectState = (Int, Int)

-- | Create Bisect PatchTree from the RL
patchTreeFromRL :: (Patchy p) => RL p C(x y) -> PatchTree p C(x y)
patchTreeFromRL (l :<: NilRL) = Leaf l
patchTreeFromRL xs = case splitAtRL (lengthRL xs `div` 2) xs of
                       (l :< r) -> Fork (patchTreeFromRL l) (patchTreeFromRL r)

-- | Convert PatchTree back to RL
patchTree2RL :: (Patchy p) => PatchTree p C(x y) -> RL p C(x y)
patchTree2RL (Leaf p)   = p :<: NilRL
patchTree2RL (Fork l r) = (patchTree2RL l) +<+ (patchTree2RL r)

-- | Iterate the Patch Tree
trackNextBisect :: (IsHunk p, Conflict p, PatchListFormat p, Patchy p)
                => [DarcsFlag] -> BisectState -> IO ExitCode -> BisectDir -> PatchTree p C(x y) -> IO ()
trackNextBisect opts (dnow, dtotal) test dir (Fork l r) = do
  putStr ("Trying " ++ show dnow ++ "/" ++ show dtotal ++ " sequences...\n")
  hFlush stdout
  case dir of
    BisectRight -> jumpHalfOnRight opts l  -- move in temporary repo
    BisectLeft  -> jumpHalfOnLeft  opts r  -- within given direction
  test_result <- test -- execute test on repo
  case test_result of
    ExitSuccess -> trackNextBisect opts (dnow+1, dtotal) test BisectLeft l  -- continue left  (to the present)
    _           -> trackNextBisect opts (dnow+1, dtotal) test BisectRight r -- continue right (to the past)
trackNextBisect _ _ _ _ (Leaf p) = do
  putStrLn ("Last recent patch that fails the test (assuming monotony in the given range):")
  putDocLn (description p)

jumpHalfOnRight :: (IsHunk p, Conflict p, PatchListFormat p, Patchy p) => [DarcsFlag] -> PatchTree p C(x y) -> IO ()
jumpHalfOnRight opts l = unapplyRL ps >> makeScriptsExecutable opts ps
  where ps = patchTree2RL l

jumpHalfOnLeft :: (IsHunk p, Conflict p, PatchListFormat p, Patchy p) => [DarcsFlag] -> PatchTree p C(x y) -> IO ()
jumpHalfOnLeft opts r = applyRL p >> makeScriptsExecutable opts p
  where p = patchTree2RL r

applyRL :: (Invert p, ShowPatch p, Apply p) => RL p C(x y) -> IO ()
applyRL   patches = sequence_ (mapFL safeApply (reverseRL $ patches))

unapplyRL :: (Invert p, ShowPatch p, Apply p) => RL p C(x y) -> IO ()
unapplyRL patches = sequence_ (mapRL (safeApply . invert) patches)

safeApply :: (Invert p, ShowPatch p, Apply p) => p C(x y) -> IO ()
safeApply p = apply p `catch` (\msg -> fail ("Bad patch (during trackdown --bisect):\n" ++ show msg))

\end{code}

Trackdown is helpful for locating when something was broken.  It creates
a temporary directory with the latest repository content in it and cd to it.
First, and only once, it runs the initialization command if any,
for example
\begin{verbatim}
'autoconf; ./configure >/dev/null'
\end{verbatim}
Then it runs the test command, for example
\begin{verbatim}
'make && cd tests && sh /tmp/test.sh'
\end{verbatim}
While the test command exits with an error return code, darcs
``unapplies'' one patch from the version controlled files to retrieve
an earlier version, and repeats the test command.  If the test command
finally succeeds, the name of the hunted down patch is found in the
output before the last test run.

The \verb!--bisect! variant of trackdown can be useful when the sought after
patch is likely buried deep in the repository history; however, it currently
requires an potentially expensive process of applying or unapplying half the
repository's patches at a time. You may often find the straightforward linear
trackdown to be more efficient in practice.

\subsubsection{Example usage}
If you want to find the last version of darcs that had a FIXME note in the
file Record.lhs, you could run
\begin{verbatim}
% darcs trackdown 'grep FIXME Record.lhs'
\end{verbatim}

To find the latest version that compiles, you can run
\begin{verbatim}
% darcs trackdown 'autoconf' './configure && make'
\end{verbatim}

Trackdown can also be used to see how other features of the code changed
with time.  For example
\begin{verbatim}
% darcs trackdown 'autoconf; ./configure' \
   "make darcs > /dev/null && cd ~/darcs && time darcs check && false"
\end{verbatim}
would let you see how long `darcs check' takes to run on each previous
version of darcs that will actually compile.  The ``\verb!&& false!''
ensures that trackdown keeps going.
