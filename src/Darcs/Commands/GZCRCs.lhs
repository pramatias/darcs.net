%  Copyright (C) 2009 Ganesh Sittampalam
%
% Permission is hereby granted, free of charge, to any person
% obtaining a copy of this software and associated documentation
% files (the "Software"), to deal in the Software without
% restriction, including without limitation the rights to use, copy,
% modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be
% included in all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
% SOFTWARE.

\subsection{darcs gzcrcs}
\darcsCommand{gzcrcs}
\begin{code}
{-# LANGUAGE CPP #-}
module Darcs.Commands.GZCRCs ( gzcrcs, doCRCWarnings ) where

import Control.Arrow ( first )
import Control.Monad ( when, unless )
import Control.Monad.Trans ( liftIO )
import Control.Monad.Writer ( runWriterT, tell )
import Data.List ( intersperse )
import Data.Monoid ( Any(..), Sum(..) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.Exit ( ExitCode(..), exitWith )
import System.IO ( hPutStr, hPutStrLn, stderr )
import Data.IORef ( newIORef, readIORef, writeIORef )

import Darcs.Commands ( DarcsCommand(..), nodefaults )
import Darcs.Arguments ( DarcsFlag( Quiet, Verbose, Check, Repair, JustThisRepo ),
                        checkOrRepair, workingRepoDir, justThisRepo
                      )
import Darcs.Repository ( Repository, amInRepository, withRepository, RepoJob(..) )
import Darcs.Patch ( RepoPatch )
import Printer ( putDocLn, text )
import ByteStringUtils ( isGZFile )
import Darcs.Lock ( gzWriteAtomicFilePSs )

-- This command needs access beyond the normal repository APIs (to
-- get at the caches and inspect them directly)
-- Could move the relevant code into Darcs.Repository modules
-- but it doesn't really seem worth it.
import Darcs.Repository.InternalTypes ( extractCache )
import Darcs.Repository.Cache ( Cache(..), writable, isthisrepo, hashedFilePath, allHashedDirs )


import Darcs.Global ( getCRCWarnings, resetCRCWarnings )
import ByteStringUtils ( gzDecompress )

#include "gadts.h"

gzcrcsDescription :: String
gzcrcsDescription = "Check or repair the CRCs of compressed files in the repository."

gzcrcsHelp :: String
gzcrcsHelp = formatText
  [
   "Versions of darcs >=1.0.4 and <2.2.0 had a bug that caused compressed files " ++
   "with bad CRCs (but valid data) to be written out. CRCs were not checked on " ++
   "reading, so this bug wasn't noticed.",
   "This command inspects your repository for this corruption and optionally repairs it.",
   "By default it also does this for any caches you have configured and any other " ++
   "local repositories listed as sources of patches for this one, perhaps because of a " ++
   "lazy get. You can limit the scope to just the current repo with the --just-this-repo " ++
   "flag.",
   "Note that readonly caches, or other repositories listed as sources, " ++
   "will be checked but not repaired. Also, this command will abort if it encounters " ++
   "any non-CRC corruption in compressed files.",
   "You may wish to also run 'darcs check --complete' before repairing the corruption. " ++
   "This is not done automatically because it might result in needing to fetch extra " ++
   "patches if the repository is lazy.",
   "If there are any other problems with your repository, you can still repair the CRCs, " ++
   "but you are advised to first make a backup copy in case the CRC errors are actually " ++
   "caused by bad data and the old CRCs might be useful in recovering that data.",
   "If you were warned about CRC errors during an operation involving another repository, " ++
   "then it is possible that the other repository contains the corrupt CRCs, so you " ++
   "should arrange for that repository to also be checked/repaired."
  ]

formatText :: [String] -> String
formatText = unlines . concat . intersperse [""] . map (map unwords . para 80 . words)

-- |Take a list of words and split it up so that each chunk fits into the specified width
-- when spaces are included. Any words longer than the specified width end up in a chunk
-- of their own.
para :: Int -> [[a]] -> [[[a]]]
para w = para'
  where para' [] = []
        para' xs = uncurry (:) $ para'' w xs
        para'' r (x:xs) | w == r || length x < r = first (x:) $ para'' (r - length x - 1) xs
        para'' _ xs = ([], para' xs)

-- |This is designed for use in an atexit handler, e.g. in Darcs.RunCommand
doCRCWarnings :: Bool -> IO ()
doCRCWarnings verbose = do
   files <- getCRCWarnings
   resetCRCWarnings
   when (not . null $ files) $ do
      hPutStr stderr . formatText $
          ["",
           "Warning: CRC errors found. These are probably harmless but should " ++
           "be repaired. See 'darcs gzcrcs --help' for more information.",
           ""]
      when verbose $ hPutStrLn stderr $ unlines ("The following corrupt files were found:":files)

gzcrcs :: DarcsCommand
gzcrcs = DarcsCommand {commandProgramName = "darcs",
                       commandName = "gzcrcs",
                       commandHelp = gzcrcsHelp,
                       commandDescription = gzcrcsDescription,
                       commandExtraArgs = 0,
                       commandExtraArgHelp = [],
                       commandCommand = gzcrcsCmd,
                       commandPrereq = amInRepository,
                       commandGetArgPossibilities = return [],
                       commandArgdefaults = nodefaults,
                       commandAdvancedOptions = [],
                       commandBasicOptions = [checkOrRepair,
                                                justThisRepo,
                                                workingRepoDir
                                               ]}

gzcrcsCmd :: [DarcsFlag] -> [String] -> IO ()
gzcrcsCmd opts _ | Check `elem` opts || Repair `elem` opts = withRepository opts (RepoJob (gzcrcs' opts))
gzcrcsCmd _ _ = error "You must specify --check or --repair for gzcrcs"

gzcrcs'
   :: (RepoPatch p) => [DarcsFlag] -> Repository p C(r u t) -> IO ()
gzcrcs' opts repo = do
  let Ca locs = extractCache repo
  -- Somewhat ugly IORef use here because it's convenient, would be nicer
  -- to pre-filter the list of locs to check and then decide whether to
  -- print the message up front.
  warnRelatedRepos <- newIORef (JustThisRepo `notElem` opts)
  ((), Any checkFailed) <- runWriterT $ flip mapM_ locs $ \loc -> do
    unless (JustThisRepo `elem` opts && not (isthisrepo loc)) $ do
     let w = writable loc
     flip mapM_ allHashedDirs $ \hdir -> do
        let dir = hashedFilePath loc hdir ""
        exists <- liftIO $ doesDirectoryExist dir
        when exists $ do
           liftIO $ do
              warn <- readIORef warnRelatedRepos
              when (warn && not (isthisrepo loc)) $ do
                 writeIORef warnRelatedRepos False
                 putInfo $ text "Also checking related repos and caches; use --just-this-repo to disable."
           liftIO $ putInfo $ text $ "Checking " ++ dir ++ (if w then "" else " (readonly)")
           files <- liftIO $ getDirectoryContents dir
           ((), Sum count) <- runWriterT $ flip mapM_ files $ \file -> do
              let fn = dir ++ file
              isfile <- liftIO $ doesFileExist fn
              when isfile $ do
                 gz <- liftIO $ isGZFile fn
                 case gz of
                    Nothing -> return ()
                    Just len -> do
                       contents <- liftIO $ B.readFile fn
                       let (uncompressed, isCorrupt) = gzDecompress (Just len) . BL.fromChunks $ [contents]
                       when isCorrupt $ do
                          tell (Sum 1) -- count of files in current directory
                          liftIO $ putVerbose $ text $ "Corrupt: " ++ fn
                          when (w && Repair `elem` opts) $ liftIO $ gzWriteAtomicFilePSs fn uncompressed
           when (count > (0 :: Int)) $ do
              liftIO $ putInfo $ text $
                 "Found " ++ show count ++ " corrupt file" ++ (if count > 1 then "s" else "") ++
                 (if Repair `elem` opts then (if w then " (repaired)" else " (not repaired") else "")
              tell (Any True) -- something corrupt somewhere
  when (Check `elem` opts && checkFailed) $ exitWith $ ExitFailure 1

 where
     putInfo s = when (not $ Quiet `elem` opts) $ putDocLn s
     putVerbose s = when (Verbose `elem` opts) $ putDocLn s

\end{code}
