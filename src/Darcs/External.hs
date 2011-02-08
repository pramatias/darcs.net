{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.External (
    backupByRenaming, backupByCopying,
    copyFileOrUrl, speculateFileOrUrl, copyFilesOrUrls, copyLocal, cloneFile,
    cloneTree, cloneTreeExcept, clonePartialsTree, clonePaths,
    fetchFilePS, fetchFileLazyPS, gzFetchFilePS,
    sendEmail, generateEmail, sendEmailDoc, resendEmail,
    signString, verifyPS,
    execDocPipe, execPipeIgnoreError,
    getTermNColors,
    pipeDoc, pipeDocSSH, execSSH,
    maybeURLCmd,
    Cachable(Cachable, Uncachable, MaxAge),
    viewDoc, viewDocWith,
    sendmailPath, diffProgram, darcsProgram
  ) where

import qualified Ratified
import Data.Maybe ( isJust, isNothing, maybeToList )
import Control.Monad ( when, zipWithM_, filterM, liftM2 )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv, getProgName )
import System.IO ( hSetBinaryMode, hPutStr, hPutStrLn, hClose,
                   openBinaryFile, IOMode( ReadMode ),
                   openBinaryTempFile,
                   hIsTerminalDevice, stdout, stderr, Handle )
import System.IO.Error ( isDoesNotExistError )
import System.Posix.Files ( getSymbolicLinkStatus, isRegularFile, isDirectory )
import System.Directory ( createDirectory, getDirectoryContents,
                          doesFileExist, doesDirectoryExist,
                          renameFile, renameDirectory, copyFile,
                          findExecutable )
import System.Process ( runProcess, runInteractiveProcess, waitForProcess )
import Control.Concurrent ( forkIO, newEmptyMVar, putMVar, takeMVar )
import Control.Exception.Extensible ( bracket, try, finally, SomeException )
import Data.Char ( toUpper )
#if defined (HAVE_MAPI)
import Foreign.C ( CString, withCString )
#endif
#ifdef HAVE_MAPI
import Foreign.Ptr ( nullPtr )
import Darcs.Lock ( canonFilename, writeDocBinFile )
#endif
#ifdef HAVE_TERMINFO
import System.Console.Terminfo( tiGetNum, setupTermFromEnv, getCapability )
#endif
import System.Posix.Files ( createLink )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath.Posix ( (</>), takeDirectory, normalise )

import Darcs.Flags ( DarcsFlag( SignAs, Sign, SignSSL,
                                Verify, VerifySSL )
                   , RemoteDarcs(..) )
import Darcs.RepoPath ( AbsolutePath, toFilePath )
import Darcs.Utils ( withCurrentDirectory, breakCommand, getViewer, ortryrunning, )
import Progress ( withoutProgress, progressList, debugMessage )

import ByteStringUtils (gzReadFilePS, linesPS, unlinesPS)
import qualified Data.ByteString as B (ByteString, empty, null, readFile
            ,hGetContents, writeFile, hPut, length
            ,take, concat, drop, isPrefixOf, singleton, append)
import qualified Data.ByteString.Char8 as BC (unpack, pack)
import qualified Data.ByteString.Lazy as BL

import Darcs.Lock ( withTemp, withOpenTemp, tempdirLoc, removeFileMayNotExist )
import CommandLine ( parseCmd, addUrlencoded )
import URL ( copyUrl, copyUrlFirst, waitUrl )
import Ssh ( getSSH, copySSH, copySSHs, SSHCmd(..) )
import URL ( Cachable(..) )
import Exec ( exec, Redirect(..), withoutNonBlock )
import Darcs.URL ( isFile, isHttpUrl, isSshUrl, splitSshUrl, SshFilePath, sshUhost )
import Darcs.Utils ( catchall )
import Printer ( Doc, Printers, putDocLnWith, hPutDoc, hPutDocLn, hPutDocWith, ($$), renderPS,
                 simplePrinters,
                 text, empty, packedString, vcat, renderString )
import Darcs.Email ( formatHeader )

#ifdef HAVE_HTTP
import Network.Browser ( browse, request, setErrHandler, setOutHandler
    , setAllowRedirects )
import Network.HTTP ( RequestMethod(GET), rspCode, rspBody, rspReason
    , mkRequest )
import Network.URI ( parseURI, uriScheme )
#endif

sendmailPath :: IO String
sendmailPath = do
  l <- filterM doesFileExist $ liftM2 (</>)
                [ "/usr/sbin", "/sbin", "/usr/lib" ]
                [ "sendmail" ]
  ex <- findExecutable "sendmail"
  when (isNothing ex && null l) $ fail "Cannot find the \"sendmail\" program."
  return $ head $ maybeToList ex ++ l

diffProgram :: IO String
diffProgram = do
  l <- filterM (fmap isJust . findExecutable) [ "gdiff", "gnudiff", "diff" ]
  when (null l) $ fail "Cannot find the \"diff\" program."
  return $ head l

-- |Get the name of the darcs executable (as supplied by @getProgName@)
darcsProgram :: IO String
darcsProgram = getProgName
-- Another option: getEnv "DARCS" `catch` \_ -> getProgName

backupByRenaming :: FilePath -> IO ()
backupByRenaming = backupBy rename
 where rename x y = do
         isD <- doesDirectoryExist x
         if isD then renameDirectory x y else renameFile x y

backupByCopying :: FilePath -> IO ()
backupByCopying = backupBy copy
 where
  copy x y = do
    isD <- doesDirectoryExist x
    if isD then do createDirectory y
                   cloneTree (normalise x) (normalise y)
           else copyFile x y

backupBy :: (FilePath -> FilePath -> IO ()) -> FilePath -> IO ()
backupBy backup f =
           do hasBF <- doesFileExist f
              hasBD <- doesDirectoryExist f
              when (hasBF || hasBD) $ helper (0::Int)
  where
  helper i = do existsF <- doesFileExist next
                existsD <- doesDirectoryExist next
                if (existsF || existsD)
                   then helper (i + 1)
                   else do putStrLn $ "Backing up " ++ f ++ "(" ++ suffix ++ ")"
                           backup f next
             where next = f ++ suffix
                   suffix = "-darcs-backup" ++ show i

copyAndReadFile :: (FilePath -> IO a) -> String -> Cachable -> IO a
copyAndReadFile readfn fou _ | isFile fou = readfn fou
copyAndReadFile readfn fou cache = withTemp $ \t -> do copyFileOrUrl DefaultRemoteDarcs fou t cache
                                                       readfn t

-- | @fetchFile fileOrUrl cache@ returns the content of its argument (either a
-- file or an URL). If it has to download an url, then it will use a cache as
-- required by its second argument.
--
-- We always use default remote darcs, since it is not fatal if the remote
-- darcs does not exist or is too old -- anything that supports transfer-mode
-- should do, and if not, we will fall back to SFTP or SCP.
fetchFilePS :: String -> Cachable -> IO B.ByteString
fetchFilePS = copyAndReadFile B.readFile

-- | @fetchFileLazyPS fileOrUrl cache@ lazily reads the content of its argument
-- (either a file or an URL). Warning: this function may constitute a fd leak;
-- make sure to force consumption of file contents to avoid that. See
-- "fetchFilePS" for details.
fetchFileLazyPS :: String -> Cachable -> IO BL.ByteString
#ifdef HAVE_HTTP
fetchFileLazyPS x c = case parseURI x of
  Just x' | uriScheme x' == "http:" -> do
    rsp <- fmap snd . browse $ do
      setErrHandler . const $ return ()
      setOutHandler . const $ return ()
      setAllowRedirects True
      request $ mkRequest GET x'
    if rspCode rsp /= (2, 0, 0)
      then fail $ "fetchFileLazyPS: " ++ rspReason rsp
      else return $ rspBody rsp
  _ -> copyAndReadFile BL.readFile x c
#else
fetchFileLazyPS x c = copyAndReadFile BL.readFile x c
#endif

gzFetchFilePS :: String -> Cachable -> IO B.ByteString
gzFetchFilePS = copyAndReadFile gzReadFilePS

copyFileOrUrl :: RemoteDarcs -> FilePath -> FilePath -> Cachable -> IO ()
copyFileOrUrl _    fou out _     | isFile fou = copyLocal fou out
copyFileOrUrl _    fou out cache | isHttpUrl  fou = copyRemote fou out cache
copyFileOrUrl rd   fou out _     | isSshUrl  fou = copySSH rd (splitSshUrl fou) out
copyFileOrUrl _    fou _   _     = fail $ "unknown transport protocol: " ++ fou

speculateFileOrUrl :: String -> FilePath -> IO ()
speculateFileOrUrl fou out | isHttpUrl fou = speculateRemote fou out
                           | otherwise = return ()

copyLocal  :: String -> FilePath -> IO ()
copyLocal fou out = createLink fou out `catchall` cloneFile fou out

clonePaths :: FilePath -> FilePath -> [FilePath] -> IO ()
clonePaths source dest = mapM_ (clonePath source dest)

clonePath :: FilePath -> FilePath -> FilePath -> IO ()
clonePath source dest path
 = do let source' = source </> path
          dest' = dest </> path
      fs <- getSymbolicLinkStatus source'
      if isDirectory fs then do
          createDirectoryIfMissing True dest'
       else if isRegularFile fs then do
          createDirectoryIfMissing True (dest </> takeDirectory path)
          cloneFile source' dest'
       else fail ("clonePath: Bad file " ++ source')
   `catch` fail ("clonePath: Bad file " ++ source </> path)

clonePartialsTree :: FilePath -> FilePath -> [FilePath] -> IO ()
clonePartialsTree source dest = mapM_ (clonePartialTree source dest)

clonePartialTree :: FilePath -> FilePath -> FilePath -> IO ()
clonePartialTree source dest "" = cloneTree source dest
clonePartialTree source dest pref
 = do createDirectoryIfMissing True (dest </> takeDirectory pref)
      cloneSubTree (source </> pref) (dest </> pref)

cloneTree :: FilePath -> FilePath -> IO ()
cloneTree = cloneTreeExcept []

cloneTreeExcept :: [FilePath] -> FilePath -> FilePath -> IO ()
cloneTreeExcept except source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` (".":"..":except)) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else fail ("cloneTreeExcept: Bad source " ++ source)
   `catch` fail ("cloneTreeExcept: Bad source " ++ source)

cloneSubTree :: FilePath -> FilePath -> IO ()
cloneSubTree source dest =
 do fs <- getSymbolicLinkStatus source
    if isDirectory fs then do
        createDirectory dest
        fps <- getDirectoryContents source
        let fps' = filter (`notElem` [".", ".."]) fps
            mk_source fp = source </> fp
            mk_dest   fp = dest   </> fp
        zipWithM_ cloneSubTree (map mk_source fps') (map mk_dest fps')
     else if isRegularFile fs then do
        cloneFile source dest
     else fail ("cloneSubTree: Bad source "++ source)
    `catch` (\e -> if isDoesNotExistError e
                   then return ()
                   else ioError e)

cloneFile :: FilePath -> FilePath -> IO ()
cloneFile = copyFile

maybeURLCmd :: String -> String -> IO(Maybe(String))
maybeURLCmd what url =
  do let prot = map toUpper $ takeWhile (/= ':') url
     fmap Just (getEnv ("DARCS_" ++ what ++ "_" ++ prot))
             `catch` \_ -> return Nothing

speculateRemote :: String -> FilePath -> IO () -- speculations are always Cachable
#if defined(HAVE_CURL) || defined(HAVE_HTTP)
speculateRemote u v =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Just _ -> return () -- can't pipeline these
         Nothing -> copyUrl u v Cachable
#else
speculateRemote u _ = maybeURLCmd "GET" u >> return ()
#endif

copyRemote :: String -> FilePath -> Cachable -> IO ()
copyRemote u v cache =
    do maybeget <- maybeURLCmd "GET" u
       case maybeget of
         Nothing -> copyRemoteNormal u v cache
         Just get ->
           do let (cmd,args) = breakCommand get
              r <- exec cmd (args++[u]) (Null, File v, AsIs)
              when (r /= ExitSuccess) $
                  fail $ "(" ++ get ++ ") failed to fetch: " ++ u

copyRemoteNormal :: String -> FilePath -> Cachable -> IO ()
copyRemoteNormal u v cache = copyUrlFirst u v cache >> waitUrl u

copyFilesOrUrls :: RemoteDarcs -> FilePath -> [String] -> FilePath -> Cachable -> IO ()
copyFilesOrUrls _ dou ns out _ | isFile dou = copyLocals dou ns out
copyFilesOrUrls _ dou ns out c    | isHttpUrl  dou = copyRemotes dou ns out c
copyFilesOrUrls remote dou ns out _
  | isSshUrl  dou = copySSHs remote (splitSshUrl dou) ns out
copyFilesOrUrls _ dou _  _   _    = fail $ "unknown transport protocol: "++dou


copyLocals :: String -> [String] -> FilePath -> IO ()
copyLocals u ns d =
    doWithPatches (\n -> copyLocal (u++"/"++n) (d++"/"++n)) ns

copyRemotes :: String -> [String] -> FilePath -> Cachable -> IO()
copyRemotes u ns d cache =
    do maybeget <- maybeURLCmd "GET" u
       maybemget <- maybeURLCmd "MGET" u
       case (maybeget, maybemget) of
         (Nothing, _) -> copyRemotesNormal u ns d cache
         (Just _, Nothing) -> doWithPatches (\n -> copyRemote (u++"/"++n) (d++"/"++n) cache) ns
         (Just _, Just mget) -> mgetRemotes mget u ns d

stringToInt :: String -> Int -> Int
stringToInt num def = case reads num of [(x,"")] -> x
                                        _ -> def

mgetRemotes :: String -> String -> [String] -> FilePath -> IO()
mgetRemotes _ _ [] _ = return ()
mgetRemotes mget u ns d = do
    mgetmax <- getEnv "DARCS_MGETMAX" `catch` \_ -> return ""
    let (nsnow, nslater) = splitAt (stringToInt mgetmax 200) ns
        (cmd, args) = breakCommand mget
        urls = map (\n -> u++"/"++n) nsnow
    withCurrentDirectory d $ do
        r <- exec cmd (args++urls) (Null,Null,AsIs)
        when (r /= ExitSuccess) $
            fail $ unlines $
                ["(" ++ mget ++ ") failed to fetch files.",
                     "source directory: " ++ d,
                     "source files:"] ++ (upto 5 nsnow) ++
                     ["still to go:"] ++ (upto 5 nslater)
    mgetRemotes mget u nslater d
    where
    upto :: Integer -> [String] -> [String]
    upto _ [] = []
    upto 0 l = [ "(" ++ (show (length l)) ++ " more)" ]
    upto n (h : t) = h : (upto (n - 1) t)

copyRemotesNormal :: String -> [String] -> FilePath -> Cachable -> IO()
copyRemotesNormal u ns d cache =
    do mapM_ (\n -> copyUrl (u++"/"++n) (d++"/"++n) cache) ns
       doWithPatches (\n -> waitUrl (u++"/"++n)) ns

doWithPatches :: (String -> IO ()) -> [String] -> IO ()
doWithPatches f patches = mapM_ (\p -> seq p $ f p) $ progressList "Copying patch" patches

-- | Run a command on a remote location without passing it any input or
--   reading its output.  Return its ExitCode
execSSH :: SshFilePath -> String -> IO ExitCode
execSSH remoteAddr command =
    do (ssh, ssh_args) <- getSSH SSH remoteAddr
       debugMessage $ unwords (ssh:ssh_args++[sshUhost remoteAddr,command])
       withoutProgress $ do hid <- runProcess ssh (ssh_args++[sshUhost remoteAddr,command])
                                   Nothing Nothing Nothing Nothing Nothing
                            waitForProcess hid

pipeDoc :: String -> [String] -> Doc -> IO ExitCode
pipeDoc c args inp = withoutNonBlock $ withoutProgress $
    do debugMessage $ unwords (c:args)
       (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
       hSetBinaryMode i True
       hSetBinaryMode o True
       mvare <- newEmptyMVar
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       mvaro <- newEmptyMVar
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO ((Ratified.hGetContents o >>= -- ratify: immediately consumed
                hPutStr stdout)
               `finally` putMVar mvaro ())
       hPutDoc i inp
       hClose i
       rval <- waitForProcess pid
       takeMVar mvare
       takeMVar mvaro
       when (rval == ExitFailure 127) $
            putStrLn $ "Command not found:\n   "++ show (c:args)
       return rval

pipeDocSSH :: SshFilePath -> [String] -> Doc -> IO ExitCode
pipeDocSSH remoteAddr args input =
    do (ssh, ssh_args) <- getSSH SSH remoteAddr
       pipeDoc ssh (ssh_args++ (sshUhost remoteAddr:args)) input

sendEmail :: String -> String -> String -> String -> String -> String -> IO ()
sendEmail f t s cc scmd body =
  sendEmailDoc f t s cc scmd Nothing (text body)


generateEmail
    :: Handle  -- ^ handle to write email to
    -> String  -- ^ From
    -> String  -- ^ To
    -> String  -- ^ Subject
    -> String  -- ^ CC
    -> Doc     -- ^ body
    -> IO ()
generateEmail h f t s cc body = do
     putHeader "To" t
     putHeader "From" f
     putHeader "Subject" s
     when (not (null cc)) (putHeader "Cc" cc)
     putHeader "X-Mail-Originator" "Darcs Version Control System"
     hPutDocLn h body
  where putHeader field value
            = B.hPut h (B.append (formatHeader field value) newline)
        newline = B.singleton 10

haveSendmail :: IO Bool
haveSendmail = (sendmailPath >> return True) `catch` (\_ -> return False)

-- | Send an email, optionally containing a patch bundle
--   (more precisely, its description and the bundle itself)
sendEmailDoc
  :: String           -- ^ from
  -> String           -- ^ to
  -> String           -- ^ subject
  -> String           -- ^ cc
  -> String           -- ^ send command
  -> Maybe (Doc, Doc) -- ^ (content,bundle)
  -> Doc              -- ^ body
  -> IO ()
sendEmailDoc _ "" _ "" _ _ _ = return ()
sendEmailDoc f "" s cc scmd mbundle body =
  sendEmailDoc f cc s "" scmd mbundle body
sendEmailDoc f t s cc scmd mbundle body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= "" then do
    withOpenTemp $ \(h,fn) -> do
      generateEmail h f t s cc body
      hClose h
      withOpenTemp $ \(hat,at) -> do
        ftable' <- case mbundle of
                   Just (content,bundle) -> do
                       hPutDocLn hat $ bundle
                       return [ ('b', renderString content) , ('a', at) ]
                   Nothing ->
                       return [ ('b', renderString body) ]
        hClose hat
        let ftable = [ ('t',addressOnly t),('c',cc),('f',f),('s',s) ] ++ ftable'
        r <- execSendmail ftable scmd fn
        when (r /= ExitSuccess) $ fail ("failed to send mail to: "
                                       ++ t ++ cc_list cc
                                       ++ "\nPerhaps sendmail is not configured.")
#ifdef HAVE_MAPI
   else do
     r <- withCString t $ \tp ->
           withCString f $ \fp ->
            withCString cc $ \ccp ->
             withCString s $ \sp ->
              withOpenTemp $ \(h,fn) -> do
               hPutDoc h body
               hClose h
               writeDocBinFile "mailed_patch" body
               cfn <- canonFilename fn
               withCString cfn $ \pcfn ->
                c_send_email fp tp ccp sp nullPtr pcfn
     when (r /= 0) $ fail ("failed to send mail to: " ++ t)
#else
   else fail $ "no mail facility (sendmail or mapi) located at configure time!"
#endif
  where addressOnly a =
          case dropWhile (/= '<') a of
          ('<':a2) -> takeWhile (/= '>') a2
          _        -> a

        cc_list [] = []
        cc_list c = " and cc'ed " ++ c

resendEmail :: String -> String -> B.ByteString -> IO ()
resendEmail "" _ _ = return ()
resendEmail t scmd body = do
  use_sendmail <- haveSendmail
  if use_sendmail || scmd /= ""
   then do
    withOpenTemp $ \(h,fn) -> do
     hPutStrLn h $ "To: "++ t
     hPutStrLn h $ find_from (linesPS body)
     hPutStrLn h $ find_subject (linesPS body)
     hPutDocLn h $ fixit $ linesPS body
     hClose h
     let ftable = [('t',t)]
     r <-  execSendmail ftable scmd fn
     when (r /= ExitSuccess) $ fail ("failed to send mail to: " ++ t)
   else
#ifdef HAVE_MAPI
    fail "Don't know how to resend email with MAPI"
#else
    fail "no mail facility (sendmail or mapi) located at configure time (use the sendmail-command option)!"
#endif
  where br            = BC.pack "\r"
        darcsurl      = BC.pack "DarcsURL:"
        content       = BC.pack "Content-"
        from_start    = BC.pack "From:"
        subject_start = BC.pack "Subject:"
        fixit (l:ls)
         | B.null l = packedString B.empty $$ vcat (map packedString ls)
         | l == br = packedString B.empty $$ vcat (map packedString ls)
         | B.take 9 l == darcsurl || B.take 8 l == content
            = packedString l $$ fixit ls
         | otherwise = fixit ls
        fixit [] = empty
        find_from (l:ls) | B.take 5 l == from_start = BC.unpack l
                         | otherwise = find_from ls
        find_from [] = "From: unknown"
        find_subject (l:ls) | B.take 8 l == subject_start = BC.unpack l
                            | otherwise = find_subject ls
        find_subject [] = "Subject: (no subject)"

execSendmail :: [(Char,String)] -> String -> String -> IO ExitCode
execSendmail ftable scmd fn =
  if scmd == "" then do
     cmd <- sendmailPath
     exec cmd ["-i", "-t"] (File fn, Null, AsIs)
  else case parseCmd (addUrlencoded ftable) scmd of
         Right (arg0:opts, wantstdin) ->
           do let stdin = if wantstdin then File fn else Null
              exec arg0 opts (stdin, Null, AsIs)
         Left e -> fail $ ("failed to send mail, invalid sendmail-command: "++(show e))
         _ -> fail $ ("failed to send mail, invalid sendmail-command")

#ifdef HAVE_MAPI
foreign import ccall "win32/send_email.h send_email" c_send_email
             :: CString -> {- sender -}
                CString -> {- recipient -}
                CString -> {- cc -}
                CString -> {- subject -}
                CString -> {- body -}
                CString -> {- path -}
                IO Int
#endif

execPSPipe :: String -> [String] -> B.ByteString -> IO B.ByteString
execPSPipe c args ps = fmap renderPS
                     $ execDocPipe c args
                     $ packedString ps

execDocPipe :: String -> [String] -> Doc -> IO Doc
execDocPipe c args instr = withoutProgress $
    do (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO $ hPutDoc i instr >> hClose i
       mvare <- newEmptyMVar
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
       rval <- waitForProcess pid
       takeMVar mvare
       case rval of
         ExitFailure ec ->fail $ "External program '"++c++
                          "' failed with exit code "++ show ec
         ExitSuccess -> return $ packedString out

-- The following is needed for diff, which returns non-zero whenever
-- the files differ.
execPipeIgnoreError :: String -> [String] -> Doc -> IO Doc
execPipeIgnoreError c args instr = withoutProgress $
    do (i,o,e,pid) <- runInteractiveProcess c args Nothing Nothing
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO $ hPutDoc i instr >> hClose i
       mvare <- newEmptyMVar
-- Warning:  A do-notation statement discarded a result of type GHC.Conc.ThreadId.
       _ <- forkIO ((Ratified.hGetContents e >>= -- ratify: immediately consumed
                hPutStr stderr)
               `finally` putMVar mvare ())
       out <- B.hGetContents o
-- Warning:  A do-notation statement discarded a result of type ExitCode.
       _ <- waitForProcess pid
       takeMVar mvare
       return $ packedString out

signString :: [DarcsFlag] -> Doc -> IO Doc
signString [] d = return d
signString (Sign:_) d = signPGP [] d
signString (SignAs keyid:_) d = signPGP ["--local-user", keyid] d
signString (SignSSL idf:_) d = signSSL idf d
signString (_:os) d = signString os d

signPGP :: [String] -> Doc -> IO Doc
signPGP args t = execDocPipe "gpg" ("--clearsign":args) t

signSSL :: String -> Doc -> IO Doc
signSSL idfile t =
    withTemp $ \cert -> do
    opensslPS ["req", "-new", "-key", idfile,
               "-outform", "PEM", "-days", "365"]
                (BC.pack "\n\n\n\n\n\n\n\n\n\n\n")
                >>= opensslPS ["x509", "-req", "-extensions",
                               "v3_ca", "-signkey", idfile,
                               "-outform", "PEM", "-days", "365"]
                >>= opensslPS ["x509", "-outform", "PEM"]
                >>= B.writeFile cert
    opensslDoc ["smime", "-sign", "-signer", cert,
                "-inkey", idfile, "-noattr", "-text"] t
    where opensslDoc = execDocPipe "openssl"
          opensslPS = execPSPipe "openssl"


verifyPS :: [DarcsFlag] -> B.ByteString -> IO (Maybe B.ByteString)
verifyPS [] ps = return $ Just ps
verifyPS (Verify pks:_) ps = verifyGPG pks ps
verifyPS (VerifySSL auks:_) ps = verifySSL auks ps
verifyPS (_:os) ps = verifyPS os ps

verifyGPG :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifyGPG goodkeys s =
    withOpenTemp $ \(th,tn) -> do
      B.hPut th s
      hClose th
      rval <- exec "gpg"  ["--batch","--no-default-keyring",
                           "--keyring",fix_path $ toFilePath goodkeys, "--verify"]
                           (File tn, Null, Null)
      case rval of
          ExitSuccess -> return $ Just gpg_fixed_s
          _ -> return Nothing
      where gpg_fixed_s = let
                not_begin_signature x =
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----"
                    &&
                    x /= BC.pack "-----BEGIN PGP SIGNED MESSAGE-----\r"
                in unlinesPS $ map fix_line $ tail $ dropWhile not_begin_signature $ linesPS s
            fix_line x | B.length x < 3 = x
                       | BC.pack "- -" `B.isPrefixOf` x = B.drop 2 x
                       | otherwise = x
#if defined(WIN32)
            fix_sep c | c=='/' = '\\'   | otherwise = c
            fix_path p = map fix_sep p
#else
            fix_path p = p
#endif

verifySSL :: AbsolutePath -> B.ByteString -> IO (Maybe B.ByteString)
verifySSL goodkeys s = do
    certdata <- opensslPS ["smime", "-pk7out"] s
                >>= opensslPS ["pkcs7", "-print_certs"]
    cruddy_pk <- opensslPS ["x509", "-pubkey"] certdata
    let key_used = B.concat $ tail $
                   takeWhile (/= BC.pack"-----END PUBLIC KEY-----")
                           $ linesPS cruddy_pk
        in do allowed_keys <- linesPS `fmap` B.readFile (toFilePath goodkeys)
              if not $ key_used `elem` allowed_keys
                then return Nothing -- Not an allowed key!
                else withTemp $ \cert ->
                     withTemp $ \on ->
                     withOpenTemp $ \(th,tn) -> do
                     B.hPut th s
                     hClose th
                     B.writeFile cert certdata
                     rval <- exec "openssl" ["smime", "-verify", "-CAfile",
                                             cert, "-certfile", cert]
                                             (File tn, File on, Null)
                     case rval of
                       ExitSuccess -> Just `fmap` B.readFile on
                       _ -> return Nothing
    where opensslPS = execPSPipe "openssl"


{-
  - This function returns number of colors supported by current terminal
  - or -1 if color output not supported or error occured.
  - Terminal type determined by TERM env. variable.
  -}
getTermNColors :: IO Int
#ifdef HAVE_TERMINFO
getTermNColors = do
  t <- setupTermFromEnv
  return $ case getCapability t $ tiGetNum "colors" of
    Nothing -> (-1)
    Just x -> x
#else
getTermNColors = return (-1)
#endif

viewDoc :: Doc -> IO ()
viewDoc = viewDocWith simplePrinters

viewDocWith :: Printers -> Doc -> IO ()
viewDocWith pr msg = do
  isTerminal <- hIsTerminalDevice stdout
-- Warning:  A do-notation statement discarded a result of type ExitCode.
  _ <- if isTerminal && lengthGreaterThan (20 :: Int) (lines $ renderString msg)
     then do viewerPlusArgs <- getViewer
             let (viewer:args) = words viewerPlusArgs
             pipeDocToPager viewer args pr msg
               `ortryrunning` pipeDocToPager  "less" [] pr msg
               `ortryrunning` pipeDocToPager  "more" [] pr msg
#ifdef WIN32
               `ortryrunning` pipeDocToPager  "more.com" [] pr msg
#endif
               `ortryrunning` pipeDocToPager "" [] pr msg
     else pipeDocToPager "" [] pr msg
  return ()
              where lengthGreaterThan n _ | n <= 0 = True
                    lengthGreaterThan _ [] = False
                    lengthGreaterThan n (_:xs) = lengthGreaterThan (n-1) xs

pipeDocToPager :: String -> [String] -> Printers -> Doc -> IO ExitCode

pipeDocToPager "" _ pr inp = do
  putDocLnWith pr inp
  return ExitSuccess

pipeDocToPager c args pr inp = withoutNonBlock $ withoutProgress $ do
  tmp <- tempdirLoc
  bracket (openBinaryTempFile tmp "darcs-pager") cleanup $ \(fn,fh) ->
    do hPutDocWith pr fh inp
       hClose fh
       bracket (openBinaryFile fn ReadMode) hClose $ \h ->
         do x <- do pid <- runProcess c args Nothing Nothing (Just h) Nothing Nothing
                    waitForProcess pid
            when (x == ExitFailure 127) $
                 putStrLn $ "Command not found:\n   "++ show (c:args)
            return x
  where
-- Warning:  A do-notation statement discarded a result of type Either SomeException ().
    cleanup (f,h) = do _ <- try (hClose h) :: IO (Either SomeException ())
                       removeFileMayNotExist f
