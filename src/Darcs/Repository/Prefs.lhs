%  Copyright (C) 2002-2003 David Roundy
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


\begin{code}
{-# LANGUAGE CPP #-}

#include "gadts.h"

module Darcs.Repository.Prefs ( addToPreflist, getPreflist, setPreflist,
                   getGlobal, environmentHelpHome,
                   defaultrepo, setDefaultrepo,
                   getPrefval, setPrefval, changePrefval,
                   defPrefval,
                   writeDefaultPrefs,
                   boringRegexps, boringFileFilter, darcsdirFilter,
                   FileType(..), filetypeFunction,
                   getCaches,
                   binariesFileHelp, boringFileHelp,
                   globalCacheDir
                 ) where

import System.IO.Error ( isDoesNotExistError )
import Control.Monad ( unless, when, mplus )
import Text.Regex ( Regex, mkRegex, matchRegex, )
import Data.Char ( toUpper )
import Data.Maybe ( isJust, fromMaybe, mapMaybe )
import Data.List ( nub, isPrefixOf, union, sortBy )
import System.Directory ( getAppUserDataDirectory )
import System.FilePath.Posix ( (</>) )
import System.Environment ( getEnvironment )

import Darcs.Flags ( DarcsFlag( NoCache, NoSetDefault, DryRun, RemoteRepo ) )
import Darcs.RepoPath ( AbsolutePath, ioAbsolute, toFilePath, getCurrentDirectory )
import Darcs.Utils ( catchall, stripCr )
import Darcs.IO ( ReadableDirectory(..), WriteableDirectory(..) )
import Darcs.Patch.FileName ( fp2fn )
import Darcs.External ( gzFetchFilePS, Cachable( Cachable ) )
import qualified Data.ByteString.Char8 as BC ( unpack )
import qualified Data.ByteString       as B  ( empty )
import Darcs.Global ( darcsdir )
import Darcs.Repository.Cache ( Cache(..), CacheType(..), CacheLoc(..),
                                WritableOrNot(..), compareByLocality )
import Darcs.URL ( isFile )
\end{code}

\section{prefs}

The \verb!_darcs! directory contains a \verb!prefs!  directory.  This
directory exists simply to hold user configuration settings specific to
this repository.  The contents of this directory are intended to be
modifiable by the user, although in some cases a mistake in such a
modification may cause darcs to behave strangely.



\input{Darcs/ArgumentDefaults.lhs}

\begin{code}
writeDefaultPrefs :: IO ()
writeDefaultPrefs =   do setPreflist "boring" defaultBoring
                         setPreflist "binaries" defaultBinaries
                         setPreflist "motd" []
\end{code}

\paragraph{repos}
The \verb!_darcs/prefs/repos! file contains a list of repositories you have
pulled from or pushed to, and is used for autocompletion of pull and push
commands in bash.  Feel free to delete any lines from this list that might
get in there, or to delete the file as a whole.

\paragraph{author}\label{author_prefs}
The \verb!_darcs/prefs/author! file contains the email address (or name) to
be used as the author when patches are recorded in this repository,
e.g.\ \verb!David Roundy <droundy@abridgegame.org>!.  This
file overrides the contents of the environment variables
\verb!$DARCS_EMAIL! and \verb!$EMAIL!.

\paragraph{boring}\label{boring}
The \verb!_darcs/prefs/boring! file may contain a list of regular
expressions describing files, such as object files, that you do not expect
to add to your project.  As an example, the boring file that I use with
my darcs repository is:
\begin{verbatim}
\.hi$
\.o$
^\.[^/]
^_
~$
(^|/)CVS($|/)
\end{verbatim}
A newly created repository has a longer boring file that
includes many common source control, backup, temporary, and compiled files.

You may want to have the boring file under version
control.  To do this you can use darcs setpref to set the value
``boringfile'' to the name of your desired boring file
(e.g.\ \verb-darcs setpref boringfile .boring-, where \verb-.boring-
is the repository path of a file
that has been
darcs added to your repository).  The boringfile preference overrides
\verb!_darcs/prefs/boring!, so be sure to copy that file to the boringfile.

You can also set up a ``boring'' regexps
file in your home directory, named \verb!~/.darcs/boring!,
(see \ref{ms_win} on MS Windows), which will be
used with all of your darcs repositories.

Any file not already managed by darcs and whose repository path (such
as \verb!manual/index.html!) matches any of
the boring regular expressions is considered boring.  The boring file is
used to filter the files provided to darcs add, to allow you to use a
simple \verb-darcs add newdir newdir/-\verb-*- % cabal haddock barfs on adjacent / *
without accidentally adding a bunch of
object files.  It is also used when the \verb!--look-for-adds! flag is
given to whatsnew or record.
Note that once a file has been added to darcs, it is not considered
boring, even if it matches the boring file filter.

\begin{code}
{-# NOINLINE defaultBoring #-}
defaultBoring :: [String]
defaultBoring = help ++
                [ "",
                  "### compiler and interpreter intermediate files",
                  "# haskell (ghc) interfaces",
                  "\\.hi$", "\\.hi-boot$", "\\.o-boot$",
                  "# object files",
                  "\\.o$","\\.o\\.cmd$",
                  "# profiling haskell",
                  "\\.p_hi$", "\\.p_o$",
                  "# haskell program coverage resp. profiling info",
                  "\\.tix$", "\\.prof$",
                  "# fortran module files",
                  "\\.mod$",
                  "# linux kernel",
                  "\\.ko\\.cmd$","\\.mod\\.c$",
                  "(^|/)\\.tmp_versions($|/)",
                  "# *.ko files aren't boring by default because they might",
                  "# be Korean translations rather than kernel modules",
                  "# \\.ko$",
                  "# python, emacs, java byte code",
                  "\\.py[co]$", "\\.elc$","\\.class$",
                  "# objects and libraries; lo and la are libtool things",
                  "\\.(obj|a|exe|so|lo|la)$",
                  "# compiled zsh configuration files",
                  "\\.zwc$",
                  "# Common LISP output files for CLISP and CMUCL",
                  "\\.(fas|fasl|sparcf|x86f)$",
                  "",
                  "### build and packaging systems",
                  "# cabal intermediates",
                  "\\.installed-pkg-config",
                  "\\.setup-config",
                  "# standard cabal build dir, might not be boring for everybody",
                  "# ^dist(/|$)",
                  "# autotools",
                  "(^|/)autom4te\\.cache($|/)", "(^|/)config\\.(log|status)$",
                  "# microsoft web expression, visual studio metadata directories",
                  "\\_vti_cnf$",
                  "\\_vti_pvt$",
                  "# gentoo tools",
                  "\\.revdep-rebuild.*",
                  "# generated dependencies",
                  "^\\.depend$",
                  "",
                  "### version control systems",
                  "# cvs",
                  "(^|/)CVS($|/)","\\.cvsignore$",
                  "# cvs, emacs locks",
                  "^\\.#",
                  "# rcs",
                  "(^|/)RCS($|/)", ",v$",
                  "# subversion",
                  "(^|/)\\.svn($|/)",
                  "# mercurial",
                  "(^|/)\\.hg($|/)",
                  "# git",
                  "(^|/)\\.git($|/)",
                  "# bzr",
                  "\\.bzr$",
                  "# sccs",
                  "(^|/)SCCS($|/)",
                  "# darcs",
                  "(^|/)"++darcsdir++"($|/)", "(^|/)\\.darcsrepo($|/)",
                  "^\\.darcs-temp-mail$",
                  "-darcs-backup[[:digit:]]+$",
                  "# gnu arch",
                  "(^|/)(\\+|,)",
                  "(^|/)vssver\\.scc$",
                  "\\.swp$","(^|/)MT($|/)",
                  "(^|/)\\{arch\\}($|/)","(^|/).arch-ids($|/)",
                  "# bitkeeper",
                  "(^|/)BitKeeper($|/)","(^|/)ChangeSet($|/)",
                  "",
                  "### miscellaneous",
                  "# backup files",
                  "~$","\\.bak$","\\.BAK$",
                  "# patch originals and rejects",
                  "\\.orig$", "\\.rej$",
                  "# X server",
                  "\\..serverauth.*",
                  "# image spam",
                  "\\#", "(^|/)Thumbs\\.db$",
                  "# vi, emacs tags",
                  "(^|/)(tags|TAGS)$",
                  "#(^|/)\\.[^/]",
                  "# core dumps",
                  "(^|/|\\.)core$",
                  "# partial broken files (KIO copy operations)",
                  "\\.part$",
                  "# waf files, see http://code.google.com/p/waf/",
                  "(^|/)\\.waf-[[:digit:].]+-[[:digit:]]+($|/)",
                  "(^|/)\\.lock-wscript$",
                  "# mac os finder",
                  "(^|/)\\.DS_Store$" ]
 where
  help = map ("# "++) boringFileHelp

boringFileHelp :: [String]
boringFileHelp =
 [ "This file contains a list of extended regular expressions, one per"
 , "line. A file path matching any of these expressions will be filtered"
 , "out during `darcs add', or when the `--look-for-adds' flag is passed"
 , "to `darcs whatsnew' and `record'.  The entries in ~/.darcs/boring (if"
 , "it exists) supplement those in this file."
 , ""
 , "Blank lines, and lines beginning with an octothorpe (#) are ignored."
 , "See regex(7) for a description of extended regular expressions."
 ]

darcsdirFilter :: [FilePath] -> [FilePath]
darcsdirFilter = filter (not.isDarcsdir)
isDarcsdir :: FilePath -> Bool
isDarcsdir ('.':'/':f) = isDarcsdir f
isDarcsdir "." = True
isDarcsdir "" = True
isDarcsdir ".." = True
isDarcsdir "../" = True
isDarcsdir "_darcs" = True
isDarcsdir fp = "_darcs/" `isPrefixOf` fp

-- | The path of the global preference directory; @~/.darcs@ on Unix,
-- and @%APPDATA%/darcs@ on Windows.
globalPrefsDir :: IO (Maybe FilePath)
globalPrefsDir = do
  env <- getEnvironment
  case lookup "DARCS_TESTING_PREFS_DIR" env of
    Just d -> return (Just d)
    Nothing -> fmap Just (getAppUserDataDirectory "darcs")
               `catchall` (return Nothing)

environmentHelpHome :: ([String], [String])
environmentHelpHome = (["HOME", "APPDATA"], [
 "Per-user preferences are set in $HOME/.darcs (on Unix) or",
 "%APPDATA%/darcs (on Windows).  This is also the default location of",
 "the cache."])

getGlobal :: String -> IO [String]
getGlobal f = do
  dir <- globalPrefsDir
  case dir of
    (Just d) -> getPreffile $ d </> f
    Nothing -> return []

globalCacheDir :: IO (Maybe FilePath)
globalCacheDir = slash_cache `fmap` globalPrefsDir
  where slash_cache = fmap (</> "cache")

boringRegexps :: IO [Regex]
boringRegexps = do
    borefile <- defPrefval "boringfile" (darcsdir ++ "/prefs/boring")
    bores <- getPrefLines borefile `catchall` return []
    gbs <- getGlobal "boring"
    return $ map mkRegex $ bores ++ gbs

boringFileFilter :: IO ([FilePath] -> [FilePath])
boringFileFilter = fmap actualBoringFileFilter boringRegexps

noncomments :: [String] -> [String]
noncomments ss = filter is_ok ss
                 where is_ok "" = False
                       is_ok ('#':_) = False
                       is_ok _ = True

getPrefLines :: ReadableDirectory m => FilePath -> m [String]
getPrefLines f = (notconflicts . noncomments . map stripCr . lines)
              `fmap` mReadBinFile (fp2fn f)
    where notconflicts = filter nc
          startswith [] _ = True
          startswith (x:xs) (y:ys) | x == y = startswith xs ys
          startswith _ _ = False
          nc l | startswith "v v v v v v v" l = False
          nc l | startswith "*************" l = False
          nc l | startswith "^ ^ ^ ^ ^ ^ ^" l = False
          nc _ = True

-- | From a list of paths, filter out any that are within @_darcs@ or
-- match a boring regexp.
actualBoringFileFilter :: [Regex] -> [FilePath] -> [FilePath]
actualBoringFileFilter regexps files = filter (not . boring . normalize) files
    where boring file = isDarcsdir file ||
                        any (\regexp -> isJust $ matchRegex regexp file) regexps

normalize :: FilePath -> FilePath
normalize ('.':'/':f) = normalize f
normalize f = normalize_helper $ reverse f
              where
              normalize_helper ('/':rf) = normalize_helper rf
              normalize_helper rf = reverse rf
\end{code}

\paragraph{binaries}
The \verb!_darcs/prefs/binaries! file may contain a list of regular
expressions describing files that should be treated as binary files rather
than text files. Darcs automatically treats files containing
\verb!^Z\! or \verb!'\0'! within the first 4096 bytes as being binary files.
You probably will want to have the binaries file under
version control.  To do this you can use darcs setpref to set the value
``binariesfile'' to the name of your desired binaries file
(e.g.\ \verb'darcs setpref binariesfile ./.binaries', where
\verb'.binaries' is a file that has been
darcs added to your repository).  As with the boring file, you can also set
up a \verb!~/.darcs/binaries! file if you like
(see \ref{ms_win} on MS Windows).

\begin{code}
data FileType = BinaryFile | TextFile
                deriving (Eq)

{-# NOINLINE defaultBinaries #-}
-- | The lines that will be inserted into @_darcs/prefs/binaries@ when
-- @darcs init@ is run.  Hence, a list of comments, blank lines and
-- regular expressions (ERE dialect).
--
-- Note that while this matches .gz and .GZ, it will not match .gZ,
-- i.e. it is not truly case insensitive.
defaultBinaries :: [String]
defaultBinaries = help ++
                   ["\\.(" ++ e ++ "|" ++ map toUpper e ++ ")$" | e <- extensions ]
    where extensions = ["a","bmp","bz2","doc","elc","exe","gif","gz","iso",
                        "jar","jpe?g","mng","mpe?g","p[nbgp]m","pdf","png",
                        "pyc","so","tar","tgz","tiff?","z","zip"]
          help = map ("# "++) binariesFileHelp

binariesFileHelp :: [String]
binariesFileHelp =
  ["This file contains a list of extended regular expressions, one per",
   "line.  A file path matching any of these expressions is assumed to",
   "contain binary data (not text).  The entries in ~/.darcs/binaries (if",
   "it exists) supplement those in this file.",
   "",
   "Blank lines, and lines beginning with an octothorpe (#) are ignored.",
   "See regex(7) for a description of extended regular expressions."]

filetypeFunction :: IO (FilePath -> FileType)
filetypeFunction = do
    binsfile <- defPrefval "binariesfile" (darcsdir ++ "/prefs/binaries")
    bins <- getPrefLines binsfile `catch`
             (\e-> if isDoesNotExistError e then return [] else ioError e)
    gbs <- getGlobal "binaries"
    let regexes = map mkRegex (bins ++ gbs)
    let isbin f = any (\r -> isJust $ matchRegex r f) regexes
        ftf f = if isbin $ normalize f then BinaryFile else TextFile
        in
        return ftf

-- this avoids a circular dependency with Repository
prefsDirectory :: ReadableDirectory m => m String
prefsDirectory =
    do darcs <- mDoesDirectoryExist $ fp2fn darcsdir
       if darcs
          then return $ darcsdir ++ "/prefs/"
          else fail $ "Directory " ++ darcsdir ++ "/ does not exist!"

withPrefsDirectory :: ReadableDirectory m => (String -> m ()) -> m ()
withPrefsDirectory j = do prefs <- prefsDirectory `mplus` return "x"
                          when (prefs /= "x") $ j prefs

addToPreflist :: WriteableDirectory m => String -> String -> m ()
addToPreflist p s = withPrefsDirectory $ \prefs -> do
  hasprefs <- mDoesDirectoryExist $ fp2fn prefs
  unless hasprefs $ mCreateDirectory $ fp2fn prefs
  pl <- getPreflist p
  mWriteBinFile (fp2fn $ prefs ++ p) $ unlines $ union [s] pl

getPreflist :: ReadableDirectory m => String -> m [String]
getPreflist p =  do prefs <- prefsDirectory `mplus` return "x"
                    if prefs /= "x" then getPreffile $ prefs ++ p
                                    else return []

getPreffile :: ReadableDirectory m => FilePath -> m [String]
getPreffile f = do
  hasprefs <- mDoesFileExist (fp2fn f)
  if hasprefs
    then getPrefLines f
    else return []

setPreflist :: WriteableDirectory m => String -> [String] -> m ()
setPreflist p ls = withPrefsDirectory $ \prefs -> do
  haspref <- mDoesDirectoryExist $ fp2fn prefs
  when haspref $ mWriteBinFile (fp2fn $ prefs ++ p) (unlines ls)

defPrefval :: String -> String -> IO String
defPrefval p d = do
  pv <- getPrefval p
  return $ fromMaybe d pv

getPrefval :: ReadableDirectory m => String -> m (Maybe String)
getPrefval p =
    do pl <- getPreflist "prefs"
       return $
        case map snd $ filter ((==p).fst) $ map (break (==' ')) pl of
           [val] ->
                case words val of
                    [] -> Nothing
                    _ -> Just $ tail val
           _ -> Nothing

setPrefval :: WriteableDirectory m => String -> String -> m ()
setPrefval p v =  do pl <- getPreflist "prefs"
                     setPreflist "prefs" $
                       filter ((/=p) . fst . break (==' ')) pl ++ [p++" "++v]

changePrefval :: WriteableDirectory m => String -> String -> String -> m ()
changePrefval p f t =
    do pl <- getPreflist "prefs"
       ov <- getPrefval p
       let newval = case ov of
                        Nothing -> t
                        Just old -> if old == f then t else old
       setPreflist "prefs" $
                    filter ((/=p) . fst . break(==' ')) pl ++ [p++" "++newval]

defaultrepo :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
defaultrepo opts _ [] =
  do let fixR r | not (isFile r) = return r
                | otherwise = do absr <- ioAbsolute r
                                 return $ toFilePath absr
     case [r | RemoteRepo r <- opts] of
       [] -> do defrepo <- getPreflist "defaultrepo"
                case defrepo of
                  [r] -> (:[]) `fmap` fixR r
                  _ -> return []
       rs -> mapM fixR rs
defaultrepo _ _ r = return r

setDefaultrepo :: String -> [DarcsFlag] -> IO ()
setDefaultrepo r opts =  do olddef <- getPreflist "defaultrepo"
                            let doit = NoSetDefault `notElem` opts && greenLight
                                greenLight = wetRun
                                           && not rIsTmp
                                           && (olddef /= [r] || olddef == [])
                            if doit
                               then setPreflist "defaultrepo" [r]
                               else when greenLight $ putStr . unlines $
                                      -- the nuance here is that we should only notify when the
                                      -- reason we're not setting default is the --no-set-default
                                      -- flag, not the various automatic show stoppers
                                      [ "HINT: if you want to change the default remote repository to"
                                      , "      " ++ r ++ ","
                                      , "      quit now and issue the same command with the --set-default flag."
                                      ]
                            addToPreflist "repos" r
                         `catchall` return () -- we don't care if this fails!
 where
  wetRun = DryRun `notElem` opts
  rIsTmp = r `elem` [x | RemoteRepo x <- opts]
\end{code}

\paragraph{email}
The \verb!_darcs/prefs/email! file is used to provide the e-mail address for your
repository that others will use when they \verb!darcs send! a patch back to you.
The contents of the file should simply be an e-mail address.


\paragraph{sources}
The \verb!_darcs/prefs/sources! file is used to indicate alternative
locations from which to download patches when using a ``hashed''
repository.  This file contains lines such as:
\begin{verbatim}
cache:/home/droundy/.darcs/cache
readonly:/home/otheruser/.darcs/cache
repo:http://darcs.net
\end{verbatim}
This would indicate that darcs should first look in
\verb!/home/droundy/.darcs/cache! for patches that might be missing, and if
the patch isn't there, it should save a copy there for future use.  In that
case, darcs will look in \verb!/home/otheruser/.darcs/cache! to see if that
user might have downloaded a copy, but won't try to save a copy there, of
course.  Finally, it will look in \verb!http://darcs.net!.  Note that the
\verb!sources! file can also exist in \verb!~/.darcs/!.  Also note that the
sources mentioned in your \verb!sources! file will be tried \emph{before}
the repository you are pulling from.  This can be useful in avoiding
downloading patches multiple times when you pull from a remote repository
to more than one local repository.

A global cache is enabled by default in your home directory.  The cache
allows darcs to avoid re-downloading patches (for example, when doing a
second darcs get of the same repository), and also allows darcs to use hard
links to reduce disk usage.

Note that the cache directory should reside on the same filesystem as your
repositories, so you may need to vary this.  You can also use multiple
cache directories on different filesystems, if you have several filesystems
on which you use darcs.

\begin{code}
getCaches :: [DarcsFlag] -> String -> IO Cache
getCaches opts repodir =
    do here <- parsehs `fmap` getPreffile (darcsdir ++ "/prefs/sources")
       there <- (parsehs . lines . BC.unpack) `fmap`
                (gzFetchFilePS (repodir </> darcsdir </> "prefs/sources") Cachable
                 `catchall` return B.empty)
       globalcachedir <- globalCacheDir
       let globalcache = case (nocache,globalcachedir) of
                           (True,_) -> []
                           (_,Just d) -> [Cache Directory Writable d]
                           _ -> []
       globalsources <- parsehs `fmap` getGlobal "sources"
       thisdir <- getCurrentDirectory
       let thisrepo = [Cache Repo Writable $ toFilePath thisdir]
       let tempCache = nub $ thisrepo ++ globalcache ++ globalsources ++
                  here ++ [Cache Repo NotWritable repodir] ++ filterExternalSources there
       return $ Ca $ sortBy compareByLocality tempCache
      where
            parsehs = mapMaybe readln . noncomments
            readln l | "repo:"     `isPrefixOf` l = Just (Cache Repo NotWritable (drop 5 l))
                     | "thisrepo:" `isPrefixOf` l = Just (Cache Repo Writable    (drop 9 l))
                     | nocache = Nothing
                     | "cache:"    `isPrefixOf` l = Just (Cache Directory Writable    (drop 6 l))
                     | "readonly:" `isPrefixOf` l = Just (Cache Directory NotWritable (drop 9 l))
                     | otherwise = Nothing
            nocache = NoCache `elem` opts
            filterExternalSources there = if isFile repodir
                                          then
                                            there
                                          else
                                            filter (not . isFile . cacheSource) there
\end{code}

