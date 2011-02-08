{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}
module Main ( main ) where

import qualified Darcs.Test.Misc
import qualified Darcs.Test.Patch
import qualified Darcs.Test.Email

import Control.Monad (when)
import Data.List ( isPrefixOf, isSuffixOf, sort )
import qualified Data.ByteString.Char8 as B
import System.Console.CmdLib
import System.FilePath( takeDirectory, takeBaseName, isAbsolute )
import Test.Framework.Providers.API
import Test.Framework
import Shellish hiding ( liftIO, run )
import qualified Shellish

doUnit :: IO [Test]
doUnit = do
  putStr Darcs.Test.Patch.testInfo
  return unitTests

-- | This is the big list of tests that will be run using testrunner.
unitTests :: [Test]
unitTests =
  [ Darcs.Test.Email.testSuite
  , Darcs.Test.Misc.testSuite
  , Darcs.Test.Patch.testSuite
  ]

-- ----------------------------------------------------------------------
-- shell tests
-- ----------------------------------------------------------------------

data Format = Hashed | Darcs2 | OldFashioned deriving Show
data Running = Running deriving Show
data Result = Success | Skipped | Failed String

instance Show Result where
  show Success = "Success"
  show Skipped = "Skipped"
  show (Failed f) = unlines (map ("| " ++) $ lines f)

instance TestResultlike Running Result where
  testSucceeded Success = True
  testSucceeded Skipped = True
  testSucceeded _ = False

data ShellTest = ShellTest { format :: Format
                           , testfile :: FilePath
                           , testdir  :: Maybe FilePath -- ^ only if you want to set it explicitly
                           , _darcspath :: FilePath
                           }

runtest' :: ShellTest -> FilePath -> ShIO Result
runtest' (ShellTest fmt _ _ dp) srcdir =
  do wd <- pwd
     setenv "HOME" wd
     setenv "TESTDATA" (srcdir </> "tests" </> "data")
     setenv "TESTBIN" (srcdir </> "tests" </> "bin")
     setenv "DARCS_TESTING_PREFS_DIR" $ wd </> ".darcs"
     setenv "EMAIL" "tester"
     setenv "DARCS_DONT_COLOR" "1"
     setenv "DARCS_DONT_ESCAPE_ANYTHING" "1"
     getenv "PATH" >>= setenv "PATH" . ((takeDirectory dp ++ ":") ++)
     setenv "DARCS" dp
     mkdir ".darcs"
     writefile ".darcs/defaults" defaults
-- Warning:  A do-notation statement discarded a result of type String.
     _ <- Shellish.run "bash" [ "test" ]
     return Success
   `catch_sh` \e -> case e of
      RunFailed _ 200 _ -> return Skipped
      RunFailed _ _   _ -> Failed <$> B.unpack <$> lastOutput
  where defaults = unlines ["ALL " ++ fmtstr, "send no-edit-description", "ALL ignore-times"]
        fmtstr = case fmt of
                  Darcs2 -> "darcs-2"
                  Hashed -> "hashed"
                  OldFashioned -> "old-fashioned-inventory"

runtest :: ShellTest -> ShIO Result
runtest t =
 withTmp $ \dir -> do
  cp "tests/lib" dir
  cp ("tests" </> testfile t) (dir </> "test")
  srcdir <- pwd
  silently $ sub $ cd dir >> runtest' t srcdir
 where
  withTmp =
   case testdir t of
     Just dir -> \job -> do
       let d = (dir </> show (format t) </> takeBaseName (testfile t))
       mkdir_p d
       job d
     Nothing  -> withTmpDir

instance Testlike Running Result ShellTest where
  testTypeName _ = "Shell"
  runTest _ test = runImprovingIO $ do yieldImprovement Running
                                       liftIO (shellish $ runtest test)

shellTest :: FilePath -> Format -> Maybe FilePath -> String -> Test
shellTest dp fmt tdir file = Test (file ++ " (" ++ show fmt ++ ")") $ ShellTest fmt file tdir dp

findShell :: FilePath -> Maybe FilePath -> Bool -> ShIO [Test]
findShell dp tdir isFailing =
  do files <- sort <$> grep relevant <$> grep (".sh" `isSuffixOf`) <$> ls "tests"
     return [ shellTest dp fmt tdir file
            | fmt <- [ Darcs2, Hashed, OldFashioned ]
            , file <- files ]
  where relevant = (if isFailing then id else not) . ("failing-" `isPrefixOf`)

findNetwork :: FilePath -> Maybe FilePath -> ShIO [Test]
findNetwork dp tdir =
  do files <- sort <$> grep (".sh" `isSuffixOf`) <$> ls "tests/network"
     return [ shellTest dp Darcs2 tdir ("network" </> file) | file <- files ]

-- ----------------------------------------------------------------------
-- harness
-- ----------------------------------------------------------------------

data Config = Config { failing :: Bool
                     , shell :: Bool
                     , network :: Bool
                     , unit :: Bool
                     , darcs :: String
                     , tests :: [String]
                     , testDir :: Maybe FilePath
                     , plain :: Bool
                     , threads :: Int }
            deriving (Data, Typeable, Eq)

instance Attributes Config where
  attributes _ = group "Options"
    [ failing %> Help "Run the failing (shell) tests."
    , shell %> Help "Run the passing, non-network shell tests." %+ Default True
    , network %> Help "Run the network shell tests."
    , unit %> Help "Run the unit tests." %+ Default True
    , tests %> Help "Pattern to limit the tests to run." %+ short 't'
    , testDir %> Help "Directory to run tests in" %+ Default (Nothing :: Maybe FilePath)
    , plain %> Help "Use plain-text output."
    , threads %> Default (1 :: Int) %+ short 'j' ]

data DarcsTest = DarcsTest deriving Typeable
instance Command DarcsTest (Record Config) where
  run _ conf _ = do
    let args = [ "-j", show $ threads conf ] ++ concat [ ["-t", x ] | x <- tests conf ] ++ [ "--plain" | True <- [plain conf] ]
    case testDir conf of
       Nothing -> return ()
       Just d  -> do e <- shellish (test_e d)
                     when e $ fail ("Directory " ++ d ++ " already exists. Cowardly exiting")
    when (shell conf || network conf || failing conf) $ do
      when (null $ darcs conf) $
        fail ("No darcs specified. Perhaps --darcs `pwd`/dist/build/darcs/darcs?")
      when (not (isAbsolute (darcs conf))) $
        fail ("Argument to --darcs should be an absolute path")
    ftests <- shellish $ if failing conf then findShell (darcs conf) (testDir conf) True else return []
    stests <- shellish $ if shell conf then findShell (darcs conf) (testDir conf) False else return []
    utests <- if unit conf then doUnit else return []
    ntests <- shellish $ if network conf then findNetwork (darcs conf) (testDir conf) else return []
    defaultMainWithArgs (ftests ++ stests ++ utests ++ ntests) args

main :: IO ()
main = getArgs >>= execute DarcsTest
