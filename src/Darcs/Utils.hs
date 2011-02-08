-- Various utility functions that do not belong anywhere else.

{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Darcs.Utils ( ortryrunning, nubsort, breakCommand
                   , showHexLen
                   , maybeGetEnv
                   , formatPath
                    -- * Monads
                   , firstJustIO
                    -- * User prompts
                   , askUser, askUserListItem
                   , PromptConfig(..), promptYorn, promptChar
                    -- * Text
                   , getViewer, editFile, runEditor
                   , stripCr
                    -- * Help
                   , environmentHelpEditor, environmentHelpPager
                    -- * Errors and exceptions
                   , catchall
                   , clarifyErrors, prettyException, prettyError
                   , addToErrorLoc
                    -- * Files and directories
                   , isFileReallySymlink, doesDirectoryReallyExist, doesFileReallyExist
                   , withCurrentDirectory
                   , withUMask
                    -- * Tree filtering.
                   , filterFilePaths, filterPaths
                    -- * Tree lookup.
                   , treeHas, treeHasDir, treeHasFile, treeHasAnycase
                   ) where

import Prelude hiding ( catch )
import Control.Exception.Extensible
             ( bracket, bracket_, catch, try,
               IOException, SomeException, Exception(fromException) )
import System.IO.Error ( annotateIOError, isUserError, ioeGetErrorString )

import Darcs.SignalHandler ( catchNonSignal )
import Numeric ( showHex )
import System.Directory ( doesFileExist )
import System.Exit ( ExitCode(..) )
import System.Environment ( getEnv )
import Data.Char ( toUpper, toLower, isSpace )
import Darcs.RepoPath ( FilePathLike, getCurrentDirectory, setCurrentDirectory, toFilePath )
import Data.Maybe ( isJust )
import Data.List ( group, sort )
import Control.Monad ( when, forM )
import Control.Monad.Error( MonadError )
import Exec ( execInteractive )
import Foreign.C.String ( CString, withCString )
import Foreign.C.Error ( throwErrno )
import Foreign.C.Types ( CInt )

import qualified Data.ByteString.Char8 as BSC

import System.Posix.Files( getSymbolicLinkStatus, isRegularFile, isDirectory, isSymbolicLink )

import Progress ( withoutProgress )

import System.Console.Haskeline ( runInputT, defaultSettings, getInputLine,
                                  getInputChar, outputStr, outputStrLn )
import qualified Data.ByteString as B ( readFile )

import Control.Monad.State.Strict( gets )
import Storage.Hashed.AnchoredPath( AnchoredPath(..), Name(..), isPrefix, floatPath )
import Storage.Hashed.Monad( withDirectory, fileExists, directoryExists
                           , virtualTreeMonad, currentDirectory
                           , TreeMonad )
import qualified Storage.Hashed.Monad as HS ( exists, tree )
import Storage.Hashed.Tree( Tree, listImmediate, findTree )

showHexLen :: (Integral a) => Int -> a -> String
showHexLen n x = let s = showHex x ""
                 in replicate (n - length s) ' ' ++ s

addToErrorLoc :: IOException -> String -> IOException
addToErrorLoc ioe s = annotateIOError ioe s Nothing Nothing

catchall :: IO a -> IO a -> IO a
a `catchall` b = a `catchNonSignal` (\_ -> b)

maybeGetEnv :: String -> IO (Maybe String)
maybeGetEnv s = fmap Just (getEnv s) `catchall` return Nothing -- err can only be isDoesNotExist


-- |The firstJustM returns the first Just entry in a list of monadic operations.  This is close to
--  `listToMaybe `fmap` sequence`, but the sequence operator evaluates all monadic members of the
--  list before passing it along (i.e. sequence is strict).  The firstJustM is lazy in that list
--  member monads are only evaluated up to the point where the first Just entry is obtained.
firstJustM :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstJustM [] = return Nothing
firstJustM (e:es) = e >>= (\v -> if isJust v then return v else firstJustM es)

-- |The firstJustIO is a slight modification to firstJustM: the
--  entries in the list must be IO monad operations and the
--  firstJustIO will silently turn any monad call that throws an
--  exception into Nothing, basically causing it to be ignored.
firstJustIO :: [IO (Maybe a)] -> IO (Maybe a)
firstJustIO = firstJustM . map (`catchall` return Nothing)


clarifyErrors :: IO a -> String -> IO a
clarifyErrors a e = a `catch` (\x -> fail $ unlines [prettyException x,e])

prettyException :: SomeException -> String
prettyException e | Just ioe <- fromException e, isUserError ioe = ioeGetErrorString ioe
prettyException e = show e

prettyError :: IOError -> String
prettyError e | isUserError e = ioeGetErrorString e
              | otherwise = show e

-- | Given two shell commands as arguments, execute the former.  The
-- latter is then executed if the former failed because the executable
-- wasn't found (code 127), wasn't executable (code 126) or some other
-- exception occurred.  Other failures (such as the user holding ^C)
-- do not cause the second command to be tried.
ortryrunning :: IO ExitCode -> IO ExitCode -> IO ExitCode
a `ortryrunning` b = do
  ret <- try a
  case ret of
    (Right (ExitFailure 126)) -> b -- command not executable
    (Right (ExitFailure 127)) -> b -- command not found
    (Right x) -> return x          -- legitimate success/failure
    (Left (_ :: SomeException)) -> b  -- an exception

withCurrentDirectory :: FilePathLike p => p -> IO a -> IO a
withCurrentDirectory name m =
    bracket
        (do cwd <- getCurrentDirectory
            when (toFilePath name /= "") (setCurrentDirectory name)
            return cwd)
        (\oldwd -> setCurrentDirectory oldwd `catchall` return ())
        (const m)

foreign import ccall unsafe "umask.h set_umask" set_umask
    :: CString -> IO CInt
foreign import ccall unsafe "umask.h reset_umask" reset_umask
    :: CInt -> IO CInt

withUMask :: String -> IO a -> IO a
withUMask umask job =
    do rc <- withCString umask set_umask
       when (rc < 0) (throwErrno "Couldn't set umask")
       bracket_
           (return ())
           (reset_umask rc)
           job

-- | Ask the user for a line of input.
askUser :: String    -- ^ The prompt to display
        -> IO String -- ^ The string the user entered.
askUser prompt = withoutProgress $ runInputT defaultSettings $
                    getInputLine prompt
                        >>= maybe (error "askUser: unexpected end of input") return

-- | @askUserListItem prompt xs@ enumerates @xs@ on the screen, allowing
--   the user to choose one of the items
askUserListItem :: String -> [String] -> IO String
askUserListItem prompt xs = withoutProgress $ runInputT defaultSettings $ do
  outputStr . unlines $ zipWith (\n x -> show n ++ ". " ++ x) [1::Int ..] xs
  loop
 where
  loop = do
    answer <- getInputLine prompt
                >>= maybe (error "askUser: unexpected end of input") return
    case maybeRead answer of
      Just n | n > 0 && n <= length xs -> return (xs !! (n-1))
      _ -> outputStrLn "Invalid response, try again!" >> loop

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
  [(x, rest)] | all isSpace rest -> Just x
  _         -> Nothing

stripCr :: String -> String
stripCr ""     = ""
stripCr "\r"   = ""
stripCr (c:cs) = c : stripCr cs


-- Format a path for screen output,
-- so that the user sees where the path begins and ends.
-- Could (should?) also warn about unprintable characters here.
formatPath :: String -> String
formatPath path = "\"" ++ quote path ++ "\""
    where quote "" = ""
          quote (c:cs) = if c `elem` ['\\', '"']
                         then '\\':c:quote cs
                         else c:quote cs

breakCommand :: String -> (String, [String])
breakCommand s = case words s of
                   (arg0:args) -> (arg0,args)
                   [] -> (s,[])

nubsort :: Ord a => [a] -> [a]
nubsort = map head . group . sort


-- | @editFile f@ lets the user edit a file which could but does not need
--   to already exist.  This function returns the exit code from the text
--   editor and a flag indicating if the user made any changes.
editFile :: FilePathLike p => p -> IO (ExitCode, Bool)
editFile ff = do
  old_content <- file_content
  ec <- runEditor f
  new_content <- file_content
  return (ec, new_content /= old_content)
      where f = toFilePath ff
            file_content = do
              exists <- doesFileExist f
              if exists then do content <- B.readFile f
                                return $ Just content
                        else return Nothing

runEditor :: FilePath -> IO ExitCode
runEditor f = do
  ed <- getEditor
  execInteractive ed f
       `ortryrunning` execInteractive "emacs" f
       `ortryrunning` execInteractive "emacs -nw" f
       `ortryrunning` execInteractive "nano" f
#ifdef WIN32
       `ortryrunning` execInteractive "edit" f
#endif

getEditor :: IO String
getEditor = getEnv "DARCS_EDITOR" `catchall`
             getEnv "DARCSEDITOR" `catchall`
             getEnv "VISUAL" `catchall`
             getEnv "EDITOR" `catchall` return "vi"

environmentHelpEditor :: ([String], [String])
environmentHelpEditor = (["DARCS_EDITOR", "DARCSEDITOR", "VISUAL", "EDITOR"],[
 "To edit a patch description of email comment, Darcs will invoke an",
 "external editor.  Your preferred editor can be set as any of the",
 "environment variables $DARCS_EDITOR, $DARCSEDITOR, $VISUAL or $EDITOR.",
 "If none of these are set, vi(1) is used.  If vi crashes or is not",
 "found in your PATH, emacs, emacs -nw, nano and (on Windows) edit are",
 "each tried in turn."])

getViewer :: IO String
getViewer = getEnv "DARCS_PAGER" `catchall`
             getEnv "PAGER" `catchall` return "less"

environmentHelpPager :: ([String], [String])
environmentHelpPager = (["DARCS_PAGER", "PAGER"],[
 "Darcs will sometimes invoke a pager if it deems output to be too long",
 "to fit onscreen.  Darcs will use the pager specified by $DARCS_PAGER",
 "or $PAGER.  If neither are set, `less' will be used."])

data PromptConfig = PromptConfig { pPrompt :: String
                                 , pBasicCharacters :: [Char]
                                 , pAdvancedCharacters :: [Char] -- ^ only shown on help
                                 , pDefault :: Maybe Char
                                 , pHelp    :: [Char]
                                 }

-- | Prompt the user for a yes or no
promptYorn :: [Char] -> IO Char
promptYorn p = promptChar (PromptConfig p "yn" [] Nothing [])

promptChar :: PromptConfig -> IO Char
promptChar (PromptConfig p basic_chs adv_chs md help_chs) =
  withoutProgress $ runInputT defaultSettings loopChar
 where
 chs = basic_chs ++ adv_chs
 loopChar = do
    let chars = setDefault (basic_chs ++ (if null adv_chs then "" else "..."))
        prompt = p ++ " [" ++ chars ++ "]" ++ helpStr
    a <- getInputChar prompt >>= maybe (error "promptChar: unexpected end of input")
                                    return
    case () of
     _ | a `elem` chs                   -> return a
       | a == ' ' -> case md of Nothing -> tryAgain
                                Just d  -> return d
       | a `elem` help_chs              -> return a
       | otherwise                      -> tryAgain
 helpStr = case help_chs of
           []                      -> ""
           (h:_) | null adv_chs    -> ", or " ++ (h:" for help: ")
                 | otherwise       -> ", or " ++ (h:" for more options: ")
 tryAgain = do outputStrLn "Invalid response, try again!"
               loopChar
 setDefault s = case md of Nothing -> s
                           Just d  -> map (setUpper d) s
 setUpper d c = if d == c then toUpper c else c

-- | Construct a filter from a list of AnchoredPaths, that will accept any path
-- that is either a parent or a child of any of the listed paths, and discard
-- everything else.
filterPaths :: [AnchoredPath] -> AnchoredPath -> t -> Bool
filterPaths files p _ = any (\x -> x `isPrefix` p || p `isPrefix` x) files

-- | Same as 'filterPath', but for ordinary 'FilePath's (as opposed to
-- AnchoredPath).
filterFilePaths :: [FilePath] -> AnchoredPath -> t -> Bool
filterFilePaths = filterPaths . map floatPath

-- Huh?
isFileReallySymlink :: FilePath -> IO Bool
isFileReallySymlink f = do fs <- getSymbolicLinkStatus f
                           return (isSymbolicLink fs)

doesFileReallyExist :: FilePath -> IO Bool
doesFileReallyExist f = do fs <- getSymbolicLinkStatus f
                           return (isRegularFile fs)

doesDirectoryReallyExist :: FilePath -> IO Bool
doesDirectoryReallyExist f = do fs <- getSymbolicLinkStatus f
                                return (isDirectory fs)

treeHasAnycase :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasAnycase tree path = fst `fmap` virtualTreeMonad (existsAnycase $ floatPath path) tree

existsAnycase :: (MonadError e m, Functor m, Monad m) => AnchoredPath -> TreeMonad m Bool
existsAnycase (AnchoredPath []) = return True
existsAnycase (AnchoredPath (Name x:xs)) =
  do wd <- currentDirectory
     Just tree <- gets (flip findTree wd . HS.tree)
     let subs = [ AnchoredPath [Name n] | (Name n, _) <- listImmediate tree,
                                          BSC.map toLower n == BSC.map toLower x ]
     or `fmap` forM subs (\path -> do
       file <- fileExists path
       if file then return True
               else withDirectory path (existsAnycase $ AnchoredPath xs))

treeHas :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHas tree path = fst `fmap` virtualTreeMonad (HS.exists $ floatPath path) tree

treeHasDir :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasDir tree path = fst `fmap` virtualTreeMonad (directoryExists $ floatPath path) tree

treeHasFile :: (MonadError e m, Functor m, Monad m) => Tree m -> FilePath -> m Bool
treeHasFile tree path = fst `fmap` virtualTreeMonad (fileExists $ floatPath path) tree
