\darcsCommand{put}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Put ( put ) where
import System.Exit ( ExitCode( ExitSuccess, ExitFailure ), exitWith )
import Control.Monad ( when )
import Data.Maybe ( mapMaybe )
import System.Directory ( createDirectory )
import Darcs.Commands ( DarcsCommand(..), nodefaults, putVerbose, putInfo )
import Darcs.Arguments ( DarcsFlag( UseFormat2, UseHashedInventory, UseOldFashionedInventory ),
                        applyas, matchOneContext, fixUrl,
                        networkOptions, flagToString, getInventoryChoices,
                        setScriptsExecutableOption, workingRepoDir, setDefault
                      )
import Darcs.Repository ( withRepoReadLock, RepoJob(..), patchSetToPatches, readRepo, amInRepository )
import Darcs.Repository.Format ( identifyRepoFormat,
                                 RepoProperty ( Darcs2, HashedInventory ), formatHas )
import Darcs.Patch.Bundle ( makeBundle2 )
import Darcs.Patch.Set ( PatchSet )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Witnesses.Eq ( EqCheck(..) )
import Darcs.Witnesses.Unsafe ( unsafeCoerceP )
import Darcs.Witnesses.Ordered ( RL(..), nullFL )
import Darcs.Match ( havePatchsetMatch, getOnePatchset )
import Darcs.Repository.Prefs ( getPreflist, setDefaultrepo )
import Darcs.URL ( isHttpUrl, isFile, splitSshUrl, SshFilePath(..) )
import Darcs.Utils ( withCurrentDirectory )
import Progress ( debugMessage )
import Darcs.RepoPath ( ioAbsoluteOrRemote, toPath )
import Darcs.External ( execSSH )
import Darcs.RemoteApply ( remoteApply )
import Darcs.Commands.Init ( initialize )
import Darcs.Email ( makeEmail )
import Darcs.Witnesses.Sealed ( Sealed(..), seal )
import Printer ( text )
#include "impossible.h"
#include "gadts.h"

putDescription :: String
putDescription =
 "Makes a copy of the repository"

putHelp :: String
putHelp =
 "The `darcs put' command creates a copy of the current repository.  It\n" ++
 "is currently very inefficient, so when creating local copies you\n" ++
 "should use `darcs get . x' instead of `darcs put x'.\n" ++
 "\n" ++
 "Currently this command just uses `darcs init' to create the target\n" ++
 "repository, then `darcs push --all' to copy patches to it.  Options\n" ++
 "passed to `darcs put' are passed to the init and/or push commands as\n" ++
 "appropriate.  See those commands for an explanation of each option.\n"

put ::DarcsCommand
put = DarcsCommand {commandProgramName = "darcs",
                    commandName = "put",
                    commandHelp = putHelp,
                    commandDescription = putDescription,
                    commandExtraArgs = 1,
                    commandExtraArgHelp = ["<NEW REPOSITORY>"],
                    commandCommand = putCmd,
                    commandPrereq = amInRepository,
                    commandGetArgPossibilities = getPreflist "repos",
                    commandArgdefaults = nodefaults,
                    commandAdvancedOptions = [applyas] ++ networkOptions,
                    commandBasicOptions = [matchOneContext, setScriptsExecutableOption,
                                             getInventoryChoices,
                                             setDefault True, workingRepoDir]}

putCmd :: [DarcsFlag] -> [String] -> IO ()
putCmd _ [""] = fail "Empty repository argument given to put."
putCmd opts [unfixedrepodir] =
 do
 repodir <- fixUrl opts unfixedrepodir
 -- Test to make sure we aren't trying to push to the current repo
 t_cur_absolute_repo_dir <- ioAbsoluteOrRemote "."
 t_req_absolute_repo_dir <- ioAbsoluteOrRemote repodir
 let cur_absolute_repo_dir = toPath t_cur_absolute_repo_dir
     req_absolute_repo_dir = toPath t_req_absolute_repo_dir
 when (cur_absolute_repo_dir == req_absolute_repo_dir) $
       fail "Can't put to current repository!"
 when (isHttpUrl req_absolute_repo_dir) $ error "Can't put to a URL!"

 debugMessage "Creating repository"
 putVerbose opts $ text "Creating repository"
 rf_or_e <- identifyRepoFormat "."
 rf <- case rf_or_e of Left e -> fail e
                       Right x -> return x
 let initopts = if formatHas Darcs2 rf
                then UseFormat2:filter (/= UseOldFashionedInventory) opts
                else if formatHas HashedInventory rf &&
                        not (UseOldFashionedInventory `elem` opts)
                     then UseHashedInventory:filter (/= UseFormat2) opts
                     else UseOldFashionedInventory:filter (/= UseFormat2) opts
 if isFile req_absolute_repo_dir
     then do createDirectory req_absolute_repo_dir
             withCurrentDirectory req_absolute_repo_dir $ (commandCommand initialize) initopts []
     else do -- isSshUrl req_absolute_repo_dir
             remoteInit (splitSshUrl req_absolute_repo_dir) initopts

 withCurrentDirectory cur_absolute_repo_dir $
                      withRepoReadLock opts $ RepoJob $ \repository -> (do
  setDefaultrepo req_absolute_repo_dir opts
  let doRead = if havePatchsetMatch opts
               then getOnePatchset repository opts  -- todo: make sure getOnePatchset has the right type
               else readRepo repository >>= (return . seal)
  Sealed (patchset :: PatchSet p C(Origin x1)) <- doRead
  Sealed (patchset2 :: PatchSet p C(Origin x2)) <- doRead
  IsEq <- return (unsafeCoerceP IsEq) :: IO (EqCheck C(x1 x2))
  let patches = patchSetToPatches patchset
      patches2 = patchSetToPatches patchset2
  when (nullFL patches) $ do
          putInfo opts $ text "No patches were selected to put. Nothing to be done."
          exitWith ExitSuccess
  bundle <- makeBundle2 Nothing NilRL patches patches2
  let message = if isFile req_absolute_repo_dir
                then bundle
                else makeEmail req_absolute_repo_dir [] Nothing bundle Nothing
  putVerbose opts $ text "Applying patches in new repository..."
  rval <- remoteApply opts req_absolute_repo_dir message
  case rval of ExitFailure ec -> do putStrLn $ "Apply failed!"
                                    exitWith (ExitFailure ec)
               ExitSuccess -> putInfo opts $ text "Put successful.") :: IO ()
putCmd _ _ = impossible

remoteInit :: SshFilePath -> [DarcsFlag] -> IO ()
remoteInit repo opts = do
    let args = mapMaybe (flagToString $ commandBasicOptions initialize) opts
        command = "darcs initialize --repodir='" ++ (sshRepo repo) ++ "' " ++ unwords args
    exitCode <- execSSH repo command
    when (exitCode /= ExitSuccess) $
         fail "Couldn't initialize remote repository."
\end{code}
