-- | This module is used by the push and put commands to apply the a bundle to a
-- remote repository. By remote I do not necessarily mean a repository on another
-- machine, it is just not the repository we're located in.
module Darcs.RemoteApply ( remoteApply, applyAs ) where

import System.Exit ( ExitCode )

import Darcs.Flags ( DarcsFlag( ApplyAs, Debug ), remoteDarcs )
import Darcs.Utils ( breakCommand )
import Darcs.URL ( isHttpUrl, isSshUrl, splitSshUrl, SshFilePath(..) )
import Darcs.External ( darcsProgram, pipeDoc, pipeDocSSH, maybeURLCmd )
import qualified Ssh( remoteDarcs )
import Printer ( Doc )

remoteApply :: [DarcsFlag] -> String -> Doc -> IO ExitCode
remoteApply opts repodir bundle
    = case applyAs opts of
        Nothing -> if isSshUrl repodir
                   then applyViaSsh opts (splitSshUrl repodir) bundle
                   else if isHttpUrl repodir
                        then applyViaUrl opts repodir bundle
                        else applyViaLocal opts repodir bundle
        Just un -> if isSshUrl repodir
                   then applyViaSshAndSudo opts (splitSshUrl repodir) un bundle
                   else applyViaSudo un repodir bundle

applyAs :: [DarcsFlag] -> Maybe String
applyAs (ApplyAs user:_) = Just user
applyAs (_:fs) = applyAs fs
applyAs [] = Nothing
applyViaSudo :: String -> String -> Doc -> IO ExitCode
applyViaSudo user repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc "sudo" ["-u",user,darcs,"apply","--all","--repodir",repo] bundle
applyViaLocal :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaLocal opts repo bundle =
    darcsProgram >>= \darcs ->
    pipeDoc darcs ("apply":"--all":"--repodir":repo:applyopts opts) bundle

applyViaUrl :: [DarcsFlag] -> String -> Doc -> IO ExitCode
applyViaUrl opts repo bundle =
    do maybeapply <- maybeURLCmd "APPLY" repo
       case maybeapply of
         Nothing -> applyViaLocal opts repo bundle
         Just apply ->
           do let (cmd, args) = breakCommand apply
              pipeDoc cmd (args ++ [repo]) bundle

applyViaSsh :: [DarcsFlag] -> SshFilePath -> Doc -> IO ExitCode
applyViaSsh opts repo bundle =
    pipeDocSSH repo [Ssh.remoteDarcs (remoteDarcs opts) ++" apply --all "++unwords (applyopts opts)++
                     " --repodir '"++(sshRepo repo)++"'"] bundle

applyViaSshAndSudo :: [DarcsFlag] -> SshFilePath -> String -> Doc -> IO ExitCode
applyViaSshAndSudo opts repo username bundle =
    pipeDocSSH repo ["sudo -u "++username++" "++Ssh.remoteDarcs (remoteDarcs opts)++
                     " apply --all --repodir '"++(sshRepo repo)++"'"] bundle

applyopts :: [DarcsFlag] -> [String]
applyopts opts = if Debug `elem` opts then ["--debug"] else []
