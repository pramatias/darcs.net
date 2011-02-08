{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Ssh (
  copySSH, copySSHs, SSHCmd(..), runSSH, getSSH,
  environmentHelpSsh, environmentHelpScp, environmentHelpSshPort,
  remoteDarcs
  ) where

import Prelude hiding ( lookup, catch )
import qualified Ratified( hGetContents )

import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
#ifndef WIN32
import System.Posix.Process ( getProcessID )
#else
import Darcs.Utils ( showHexLen )
import Data.Bits ( (.&.) )
import System.Random ( randomIO )
#endif
import System.IO ( Handle, hSetBinaryMode, hPutStr, hPutStrLn, hGetLine, hClose, hFlush )
import System.IO.Unsafe ( unsafePerformIO )
import System.Directory ( doesFileExist, createDirectoryIfMissing )
import Control.Monad ( when )
import System.Process ( runInteractiveProcess )

import Data.List ( isPrefixOf )
import Data.Map ( Map, empty, insert, lookup )
import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )

import Darcs.SignalHandler ( catchNonSignal )
import Darcs.Utils ( withCurrentDirectory, breakCommand, prettyException, catchall )
import Darcs.Global ( atexit, sshControlMasterDisabled, darcsdir, withDebugMode )
import Darcs.Lock ( withTemp, withOpenTemp, tempdirLoc, removeFileMayNotExist )
import Darcs.URL (SshFilePath(..), urlOf)
import Exec ( exec, Redirects, Redirect(..), )
import Progress ( withoutProgress, debugMessage, debugFail, progressList )
import Darcs.Flags( RemoteDarcs(..) )

import qualified Data.ByteString as B (ByteString, hGet, writeFile, readFile)
import qualified Data.ByteString.Char8 as BC (unpack)

{-# NOINLINE sshConnections #-}
sshConnections :: IORef (Map String (Maybe Connection))
sshConnections = unsafePerformIO $ newIORef empty

data Connection = C { inp :: !Handle, out :: !Handle, err :: !Handle, deb :: String -> IO () }

-- | @withSSHConnection rdarcs destination withconnection withoutconnection@
-- performs an action on a remote host. If we are already connected to @destination@,
-- then it does @withconnection@, else @withoutconnection@.
withSSHConnection :: String -> SshFilePath -> (Connection -> IO a) -> IO a -> IO a
withSSHConnection rdarcs repoid withconnection withoutconnection =
    withoutProgress $
    do cs <- readIORef sshConnections
       case lookup (urlOf repoid) (cs :: Map String (Maybe Connection)) of
         Just Nothing -> withoutconnection
         Just (Just c) -> withconnection c
         Nothing ->
           do mc <- do (ssh,sshargs_) <- getSSHOnly SSH
                       let sshargs = sshargs_ ++ [sshUhost repoid, rdarcs,
                                                  "transfer-mode","--repodir",sshRepo repoid]
                       debugMessage $ "ssh "++unwords sshargs
                       (i,o,e,_) <- runInteractiveProcess ssh sshargs Nothing Nothing
                       hSetBinaryMode i True
                       hSetBinaryMode o True
                       l <- hGetLine o
                       if l == "Hello user, I am darcs transfer mode"
                           then return ()
                           else debugFail "Couldn't start darcs transfer-mode on server"
                       let c = C { inp = i, out = o, err = e,
                                   deb = \s -> debugMessage ("with ssh (transfer-mode) "++sshUhost repoid++s) }
                       modifyIORef sshConnections (insert (urlOf repoid) (Just c))
                       return $ Just c
                    `catchNonSignal`
                            \e -> do debugMessage $ "Failed to start ssh connection:\n    "++
                                                    prettyException e
                                     severSSHConnection repoid
                                     debugMessage $ unlines $
                                         [ "NOTE: the server may be running a version of darcs prior to 2.0.0."
                                         , ""
                                         , "Installing darcs 2 on the server will speed up ssh-based commands."
                                         ]
                                     return Nothing
              maybe withoutconnection withconnection mc

severSSHConnection :: SshFilePath -> IO ()
severSSHConnection x = do debugMessage $ "Severing ssh failed connection to "++(sshUhost x)
                          modifyIORef sshConnections (insert (urlOf x) Nothing)

grabSSH :: SshFilePath -> Connection -> IO B.ByteString
grabSSH dest c = do
  debugMessage $ "grabSSH dest=" ++ urlOf dest
  let failwith e = do severSSHConnection dest
                        -- hGetContents is ok here because we're
                        -- only grabbing stderr, and we're also
                        -- about to throw the contents.
                      eee <- Ratified.hGetContents (err c)
                      debugFail $ e ++ " grabbing ssh file "++
                        urlOf dest++"/"++ file ++"\n"++eee
      file = sshFile dest
  deb c $ "get "++ file
  hPutStrLn (inp c) $ "get " ++ file
  hFlush (inp c)
  l2 <- hGetLine (out c)
  if l2 == "got "++file
    then do showlen <- hGetLine (out c)
            case reads showlen of
              [(len,"")] -> B.hGet (out c) len
              _ -> failwith "Couldn't get length"
    else if l2 == "error "++file
         then do e <- hGetLine (out c)
                 case reads e of
                   (msg,_):_ -> debugFail $ "Error reading file remotely:\n"++msg
                   [] -> failwith "An error occurred"
         else failwith "Error"

sshStdErrMode :: IO Redirect
sshStdErrMode = withDebugMode $ \amdebugging ->
                return $ if amdebugging then AsIs else Null

remoteDarcs :: RemoteDarcs -> String
remoteDarcs DefaultRemoteDarcs = "darcs"
remoteDarcs (RemoteDarcs x) = x

copySSH :: RemoteDarcs -> SshFilePath -> FilePath -> IO ()
copySSH remote dest to | rdarcs <- remoteDarcs remote = do
  debugMessage $ "copySSH file: " ++ urlOf dest
  withSSHConnection rdarcs dest (\c -> grabSSH dest c >>= B.writeFile to) $
              do let u = escape_dollar $ urlOf dest
                 stderr_behavior <- sshStdErrMode
                 r <- runSSH SCP dest [u,to] (AsIs,AsIs,stderr_behavior)
                 when (r /= ExitSuccess) $
                      debugFail $ "(scp) failed to fetch: " ++ u
    where {- '$' in filenames is troublesome for scp, for some reason.. -}
          escape_dollar :: String -> String
          escape_dollar = concatMap tr
           where tr '$' = "\\$"
                 tr c = [c]

copySSHs :: RemoteDarcs -> SshFilePath -> [FilePath] -> FilePath -> IO ()
copySSHs remote repo ns d | rdarcs <- remoteDarcs remote =
  withSSHConnection rdarcs repo
  (\c -> withCurrentDirectory d $
            mapM_ (\n -> grabSSH (repo {sshFile = n}) c >>= B.writeFile n) $
            progressList "Copying via ssh" ns) $
     do
      let path = sshRepo repo
          cd = "cd "++path++"/"++darcsdir++"\n"
          input = cd++(unlines $ map ("get "++) ns)
      withCurrentDirectory d $ withOpenTemp $ \(th,tn) ->
         withTemp $ \sftpoutput ->
         do hPutStr th input
            hClose th
            stderr_behavior <- sshStdErrMode
            r <- runSSH SFTP repo [] (File tn, File sftpoutput, stderr_behavior)
            let files = if length ns > 5
                          then (take 5 ns) ++ ["and "
                               ++ (show (length ns - 5)) ++ " more"]
                          else ns
                hint = if "~" `isPrefixOf` path
                         then ["sftp doesn't expand ~, use path/ instead of ~/path/"]
                         else []
            when (r /= ExitSuccess) $ do
                 outputPS <- B.readFile sftpoutput
                 debugFail $ unlines $
                          ["(sftp) failed to fetch files.",
                           "source directory: " ++ path,
                           "source files:"] ++ files ++
                          ["sftp output:",BC.unpack outputPS] ++
                          hint

-- ---------------------------------------------------------------------
-- older ssh helper functions
-- ---------------------------------------------------------------------

data SSHCmd = SSH | SCP | SFTP

instance Show SSHCmd where
  show SSH  = "ssh"
  show SCP  = "scp"
  show SFTP = "sftp"

runSSH :: SSHCmd -> SshFilePath -> [String] -> Redirects -> IO ExitCode
runSSH cmd remoteAddr postArgs redirs =
 do (ssh, args) <- getSSH cmd remoteAddr
    exec ssh (args ++ [sshUhost remoteAddr] ++ postArgs) redirs

-- | Return the command and arguments needed to run an ssh command
--   along with any extra features like use of the control master.
--   See 'getSSHOnly'
getSSH :: SSHCmd -> SshFilePath -- ^ remote path
       -> IO (String, [String])
getSSH cmd remoteAddr =
 do (ssh, ssh_args) <- getSSHOnly cmd
    cm_args <- if sshControlMasterDisabled
               then return []
               else do -- control master
                       cmPath <- controlMasterPath remoteAddr
                       hasLaunchedCm <- doesFileExist cmPath
                       when (not hasLaunchedCm) $ launchSSHControlMaster remoteAddr
                       hasCmFeature <- doesFileExist cmPath
                       return $ if hasCmFeature then [ "-o ControlPath=" ++ cmPath ] else []
    let verbosity = case cmd of
                    SCP  -> ["-q"] -- (p)scp is the only one that recognises -q
                                   -- sftp and (p)sftp do not, and plink neither
                    _    -> []
    --
    return (ssh, verbosity ++ ssh_args ++ cm_args)

-- | Return the command and arguments needed to run an ssh command.
--   First try the appropriate darcs environment variable and SSH_PORT
--   defaulting to "ssh" and no specified port.
getSSHOnly :: SSHCmd -> IO (String, [String])
getSSHOnly cmd =
 do ssh_command <- getEnv (evar cmd) `catchall` return (show cmd)
    -- port
    port <- (portFlag cmd `fmap` getEnv "SSH_PORT") `catchall` return []
    let (ssh, ssh_args) = breakCommand ssh_command
    --
    return (ssh, ssh_args ++ port)
    where
     evar SSH  = "DARCS_SSH"
     evar SCP  = "DARCS_SCP"
     evar SFTP = "DARCS_SFTP"
     portFlag SSH  x = ["-p", x]
     portFlag SCP  x = ["-P", x]
     portFlag SFTP x = ["-oPort="++x]

environmentHelpSsh :: ([String], [String])
environmentHelpSsh = (["DARCS_SSH"], [
 "Repositories of the form [user@]host:[dir] are taken to be remote",
 "repositories, which Darcs accesses with the external program ssh(1).",
 "",
 "The environment variable $DARCS_SSH can be used to specify an",
 "alternative SSH client.  Arguments may be included, separated by",
 "whitespace.  The value is not interpreted by a shell, so shell",
 "constructs cannot be used; in particular, it is not possible for the",
 "program name to contain whitespace by using quoting or escaping."])

environmentHelpScp :: ([String], [String])
environmentHelpScp = (["DARCS_SCP", "DARCS_SFTP"], [
 "When reading from a remote repository, Darcs will attempt to run",
 "`darcs transfer-mode' on the remote host.  This will fail if the",
 "remote host only has Darcs 1 installed, doesn't have Darcs installed",
 "at all, or only allows SFTP.",
 "",
 "If transfer-mode fails, Darcs will fall back on scp(1) and sftp(1).",
 "The commands invoked can be customized with the environment variables",
 "$DARCS_SCP and $DARCS_SFTP respectively, which behave like $DARCS_SSH.",
 "If the remote end allows only sftp, try setting DARCS_SCP=sftp."])

environmentHelpSshPort :: ([String], [String])
environmentHelpSshPort = (["SSH_PORT"], [
 "If this environment variable is set, it will be used as the port",
 "number for all SSH calls made by Darcs (when accessing remote",
 "repositories over SSH).  This is useful if your SSH server does not",
 "run on the default port, and your SSH client does not support",
 "ssh_config(5).  OpenSSH users will probably prefer to put something",
 "like `Host *.example.net Port 443' into their ~/.ssh/config file."])

-- | Return True if this version of ssh has a ControlMaster feature
-- The ControlMaster functionality allows for ssh multiplexing
hasSSHControlMaster :: IO Bool
hasSSHControlMaster = do
  (ssh, _) <- getSSHOnly SSH
  -- If ssh has the ControlMaster feature, it will recognise the
  -- the -O flag, but exit with status 255 because of the nonsense
  -- command.  If it does not have the feature, it will simply dump
  -- a help message on the screen and exit with 1.
  sx <- exec ssh ["-O", "an_invalid_command"] (Null,Null,Null)
  case sx of
    ExitFailure 255 -> return True
    _ -> return False

-- | Launch an SSH control master in the background, if available.
--   We don't have to wait for it or anything.
--   Note also that this will cleanup after itself when darcs exits
launchSSHControlMaster :: SshFilePath -> IO ()
launchSSHControlMaster dest = do
  hasMaster <- hasSSHControlMaster
  when hasMaster $ do
    (ssh, ssh_args) <- getSSHOnly SSH
    cmPath <- controlMasterPath dest
    removeFileMayNotExist cmPath
    -- -f : put ssh in the background once it succeeds in logging you in
    -- -M : launch as the control master for addr
    -- -N : don't run any commands
    -- -S : use cmPath as the ControlPath.  Equivalent to -oControlPath=
-- Warning:  A do-notation statement discarded a result of type ExitCode.
    _ <- exec ssh (ssh_args ++ [sshUhost dest, "-S", cmPath, "-N", "-f", "-M"]) (Null,Null,AsIs)
    atexit $ exitSSHControlMaster dest
    return ()

-- | Tell the SSH control master for a given path to exit.
exitSSHControlMaster :: SshFilePath -> IO ()
exitSSHControlMaster addr = do
  (ssh, ssh_args) <- getSSHOnly SSH
  cmPath <- controlMasterPath addr
-- Warning:  A do-notation statement discarded a result of type ExitCode.
  _ <- exec ssh (ssh_args ++ [sshUhost addr, "-S", cmPath, "-O", "exit"]) (Null,Null,Null)
  return ()

-- | Create the directory ssh control master path for a given address
controlMasterPath :: SshFilePath -- ^ remote path (foo\@bar.com:file is ok; the file part with be stripped)
                  -> IO FilePath
controlMasterPath dest = do
  let addr = sshUhost dest
  tmp <- (fmap (/// ".darcs") $ getEnv "HOME") `catchall` tempdirLoc
#ifdef WIN32
  r <- randomIO
  let suffix = (showHexLen 6 (r .&. 0xFFFFFF :: Int))
#else
  suffix <- show `fmap` getProcessID
#endif
  let tmpDarcsSsh = tmp /// "darcs-ssh"
  createDirectoryIfMissing True tmpDarcsSsh
  return $ tmpDarcsSsh /// addr ++ suffix

(///) :: FilePath -> FilePath -> FilePath
d /// f = d ++ "/" ++ f
