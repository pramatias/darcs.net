\begin{code}
{-# LANGUAGE CPP #-}
-- copyright (c) 2008 Duncan Coutts
-- portions copyright (c) 2008 David Roundy

import Prelude hiding ( catch )
import qualified Prelude

import Distribution.Simple
         ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.ModuleName( toFilePath )
import Distribution.PackageDescription
         ( PackageDescription(executables), Executable(buildInfo, exeName)
         , BuildInfo(customFieldsBI), emptyBuildInfo
         , updatePackageDescription, cppOptions, ccOptions
         , library, libBuildInfo, otherModules )
import Distribution.Package
         ( packageVersion, packageName, PackageName(..) )
import Distribution.Version
         ( Version(versionBranch) )
import Data.Version( showVersion )
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), absoluteInstallDirs, externalPackageDeps )
import Distribution.Simple.InstallDirs (mandir, CopyDest (NoCopyDest))
import Distribution.Simple.Setup
    (buildVerbosity, copyDest, copyVerbosity, fromFlag,
     haddockVerbosity, installVerbosity, sDistVerbosity)
import Distribution.Simple.BuildPaths
         ( autogenModulesDir, exeExtension )
import Distribution.System
         ( OS(Windows), buildOS )
import Distribution.Simple.Utils
    (copyFiles, createDirectoryIfMissingVerbose, rawSystemStdout,
     rewriteFile)
import Distribution.Verbosity
         ( Verbosity )
import Distribution.Text
         ( display )
import Distribution.Package (Package)

import Control.Monad ( zipWithM_, when, unless, filterM )
import Control.Exception ( bracket )
import System.Directory
    (copyFile, createDirectory, createDirectoryIfMissing,
     doesDirectoryExist, doesFileExist,
     getCurrentDirectory, getDirectoryContents,
     removeDirectoryRecursive, removeFile, setCurrentDirectory)
import System.IO (openFile, IOMode (..))
import System.Process (runProcess)
import System.IO.Error ( isDoesNotExistError )
import Data.List( isPrefixOf, isSuffixOf, sort, partition )

import System.Cmd( rawSystem )
import System.Exit( exitWith )
import System.FilePath       ( (</>), (<.>), splitDirectories, isAbsolute )
import Foreign.Marshal.Utils ( with )
import Foreign.Storable      ( peek )
import Foreign.Ptr           ( castPtr )
import Data.Word             ( Word8, Word32 )

import qualified Control.Exception as Exception

-- Handle exceptions migration. We could use extensible-exceptions
-- but Cabal can't handle package dependencies of Setup.lhs
-- automatically so it'd be disruptive for users.
-- Once we drop older GHCs we can clean up the use sites properly
-- and perhaps think about being more restrictive in which exceptions
-- are caught at each site.
#if __GLASGOW_HASKELL__ >= 610
catchAny f h = Exception.catch f (\e -> h (e :: Exception.SomeException))
#else
catchAny = Exception.catch
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks {

  buildHook = \ pkg lbi hooks flags ->
              let verb = fromFlag $ buildVerbosity flags
               in commonBuildHook buildHook pkg lbi hooks verb >>= ($ flags),

  haddockHook = \ pkg lbi hooks flags ->
                let verb = fromFlag $ haddockVerbosity flags
                 in commonBuildHook haddockHook pkg lbi hooks verb >>= ($ flags) ,

  postBuild = \ _ _ _ lbi -> buildManpage lbi,
  postCopy = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ copyVerbosity flags) (fromFlag $ copyDest flags),
  postInst = \ _ flags pkg lbi ->
             installManpage pkg lbi (fromFlag $ installVerbosity flags) NoCopyDest,

  runTests = \ args0 _ _ lbi -> do
             cwd <- getCurrentDirectory
             let isabs = isAbsolute $ buildDir lbi
                 builddir = (if isabs then id else (cwd </>)) $ buildDir lbi
                 darcs = builddir </> "darcs" </> "darcs" <.> exeExtension
                 darcstest = builddir </> "darcs-test" </> "darcs-test" <.> exeExtension
                 (flags, args1) = partition ('=' `elem`) args0
                 (what', args2) = partition (`elem` ["bugs", "network", "tests"]) args1
                 what = if null what' then ["tests"] else what'
                 opts = [ "--darcs", darcs, "--no-unit" ] ++ concat [ ["-t", x] | x <- args2 ] ++
                        [ "--network" | "network" <- what ] ++ [ "--failing" | "bugs" <- what ] ++
                        [ "--no-shell" | _ <- [()], "tests" `notElem` what ] ++
                        [ "--threads=" ++ drop 8 t | t <- flags, "threads" `isPrefixOf` t ] ++
                        [ "--plain" | "output=plain" <- flags ]
             rawSystem darcstest opts >>= exitWith
             ,

  sDistHook = \ pkg lbi hooks flags -> do
    let pkgVer = packageVersion pkg
        verb = fromFlag $ sDistVerbosity flags
    x <- versionPatches verb pkgVer
    y <- context verb pkgVer
    rewriteFile "release/distributed-version" $ show x
    rewriteFile "release/distributed-context" $ show y
    putStrLn "about to hand over"
    let pkg' = pkg { library = sanity (library pkg) }
        sanity (Just lib) = Just $ lib { libBuildInfo = sanity' $ libBuildInfo lib }
        sanity _ = error "eh"
        sanity' bi = bi { otherModules = [ m | m <- otherModules bi, toFilePath m /= "Version" ] }

    sDistHook simpleUserHooks pkg' lbi hooks flags
}

-- | For @./Setup build@ and @./Setup haddock@, do some unusual
-- things, then invoke the base behaviour ("simple hook").
commonBuildHook :: (UserHooks -> PackageDescription -> LocalBuildInfo -> t -> a)
                -> PackageDescription -> LocalBuildInfo -> t -> Verbosity -> IO a
commonBuildHook runHook pkg lbi hooks verbosity = do
  (version, state) <- determineVersion verbosity pkg

  -- Create our own context file.
  generateVersionModule verbosity pkg lbi version state

  -- Add custom -DFOO[=BAR] flags to the cpp (for .hs) and cc (for .c)
  -- invocations, doing a dance to make the base hook aware of them.
  littleEndian <- testEndianness
  let args = ("-DPACKAGE_VERSION=" ++ show' version) :
             ("-DPACKAGE_VERSION_STATE=" ++ show' state) :
             [arg | (arg, True) <-         -- include fst iff snd.
              [("-DHAVE_HTTP", "x-have-http" `elem` customFields),
               ("-DUSE_COLOR", "x-use-color" `elem` customFields),
               -- We have MAPI iff building on/for Windows.
               ("-DHAVE_MAPI", buildOS == Windows),
               ("-DBIGENDIAN", not littleEndian)]]
      bi = emptyBuildInfo { cppOptions = args, ccOptions = args }
      hbi = (Just bi, [(exeName exe, bi) | exe <- executables pkg])
      pkg' = updatePackageDescription hbi pkg
      lbi' = lbi { localPkgDescr = pkg' }
  return $ runHook simpleUserHooks pkg' lbi' hooks

  where
    customFields = map fst . customFieldsBI . buildInfo $ darcsExe
    darcsExe = head [e | e <- executables pkg, exeName e == "darcs"]
    show' :: String -> String   -- Petr was worried that we might
    show' = show                -- allow non-String arguments.
    testEndianness :: IO Bool
    testEndianness = with (1 :: Word32) $ \p -> do o <- peek $ castPtr p
                                                   return $ o == (1 :: Word8)

buildManpage :: LocalBuildInfo -> IO ()
buildManpage lbi = do
  let darcs = buildDir lbi </> "darcs/darcs"
      manpage = buildDir lbi </> "darcs/darcs.1"
  manpageHandle <- openFile manpage WriteMode
  runProcess darcs ["help","manpage"]
             Nothing Nothing Nothing (Just manpageHandle) Nothing
  return ()

installManpage :: PackageDescription -> LocalBuildInfo
                  -> Verbosity -> CopyDest -> IO ()
installManpage pkg lbi verbosity copy =
    copyFiles verbosity
              (mandir (absoluteInstallDirs pkg lbi copy) </> "man1")
              [(buildDir lbi </> "darcs", "darcs.1")]

determineVersion :: Verbosity -> PackageDescription -> IO (String, String)
determineVersion verbosity pkg = do
  let darcsVersion  =  packageVersion pkg
  numPatches <- versionPatches verbosity darcsVersion
  return (display darcsVersion, versionStateString numPatches darcsVersion)

  where
    versionStateString :: Maybe Int -> Version -> String
    versionStateString Nothing  _ = "unknown"
    versionStateString (Just 0) v = case versionBranch v of
                         x | 97 `elem` x -> "alpha " ++ show (after 97 x)
                           | 98 `elem` x -> "beta " ++ show (after 98 x)
                           | 99 `elem` x  ->
                               "release candidate " ++ show (after 99 x)
                         _ -> "release"
    versionStateString (Just 1) _ = "+ 1 patch"
    versionStateString (Just n) _ = "+ " ++ show n ++ " patches"
    after w (x:r) | w == x = head r
                  | otherwise = after w r
    after _ [] = undefined

versionPatches :: Verbosity -> Version -> IO (Maybe Int)
versionPatches verbosity darcsVersion = do
  numPatchesDarcs <- do
      out <- rawSystemStdout verbosity "darcs"
               ["changes", "--from-tag", display darcsVersion, "--count"]
      case reads (out) of
        ((n,_):_) -> return $ Just ((n :: Int) - 1)
        _         -> return Nothing
    `catchAny` \_ -> return Nothing

  numPatchesDist <- parseFile versionFile
  return $ case (numPatchesDarcs, numPatchesDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing

 where
  versionFile = "release/distributed-version"

generateVersionModule :: Verbosity -> PackageDescription -> LocalBuildInfo
                      -> String -> String -> IO ()
generateVersionModule verbosity pkg lbi version state = do
  let dir = autogenModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  ctx <- context verbosity (packageVersion pkg)
  rewriteFile (dir </> "Version.hs") $ unlines
    ["module Version where"
    ,"builddeps, version, context :: String"
    ,"version = \"" ++ version ++ " (" ++ state ++ ")\""
    ,"builddeps = " ++ (show $ formatdeps (externalPackageDeps lbi))
    ,"context = " ++ case ctx of
                       Just x -> show x
                       Nothing -> show "context not available"
    ]
  where formatdeps = unlines . map (formatone . snd)
        formatone p = case packageName p of PackageName n -> n ++ "-" ++ showVersion (packageVersion p)

context :: Verbosity -> Version -> IO (Maybe String)
context verbosity version = do
  contextDarcs <- do
      inrepo <- doesDirectoryExist "_darcs"
      unless inrepo $ fail "Not a repository."
      out <- rawSystemStdout verbosity "darcs" ["changes", "--context"]
      return $ Just out
   `catchAny` \_ -> return Nothing

  contextDist <- parseFile contextFile
  return $ case (contextDarcs, contextDist) of
             (Just x, _) -> Just x
             (Nothing, Just x) -> Just x
             (Nothing, Nothing) -> Nothing
 where contextFile = "release/distributed-context"

parseFile :: (Read a) => String -> IO (Maybe a)
parseFile f = do
  exist <- doesFileExist f
  if exist then do
             content <- readFile f -- ^ ratify readFile: we don't care here.
             case reads content of
               ((s,_):_) -> return s
               _         -> return Nothing
             else return Nothing

\end{code}
