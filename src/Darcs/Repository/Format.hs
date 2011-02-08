-- Copyright (C) 2005 David Roundy
--
-- This file is licensed under the GPL, version two or later.

{-# LANGUAGE CPP #-}

module Darcs.Repository.Format ( RepoFormat(..), RepoProperty(..), identifyRepoFormat,
                    createRepoFormat, writeRepoFormat,
                    writeProblem, readProblem, readfromAndWritetoProblem,
                    formatHas,
                  ) where

import Data.Maybe ( isJust, mapMaybe )
import Control.Monad ( mplus )

import Darcs.SignalHandler ( catchNonSignal )
import Darcs.External ( fetchFilePS, Cachable( Cachable ) )
import Darcs.Flags ( DarcsFlag ( UseFormat2, UseHashedInventory,
                                 UseOldFashionedInventory ) )
import Darcs.Lock ( writeBinFile )
import Darcs.Utils ( catchall, prettyException )
import Progress ( beginTedious, endTedious, finishedOneIO )
import Darcs.Global ( darcsdir )

import ByteStringUtils ( linesPS )
import qualified Data.ByteString.Char8 as BC (split, unpack, singleton, elemIndex, pack)
import qualified Data.ByteString       as B (ByteString, null, empty)
import qualified ByteStringUtils       as BU ( intercalate )

#include "impossible.h"

data RepoProperty = Darcs1_0 | Darcs2 | HashedInventory

-- | @RepoFormat@ is the representation of the format of a
-- repository. Each sublist corresponds to a line in the format
-- file. Each line is decomposed into words.
newtype RepoFormat = RF [[B.ByteString]] deriving ( Show )

-- | The file where the format information should be.
df :: FilePath
df = darcsdir++"/format"

-- | @identifyRepoFormat URL@ identifies the format of the repository
-- at the given address. Return @Left reason@ if it fails, where
-- @reason@ explains why we weren't able to identify the format.
identifyRepoFormat :: String -> IO (Either String RepoFormat)
identifyRepoFormat repo =
 do let k = "Identifying repository "++repo
    beginTedious k
    finishedOneIO k "format"
    dff <- fetchFilePS (repo ++ "/" ++ df) Cachable `catchall` return B.empty
    -- below is a workaround for servers that don't return a 404 on nonexistent files
    rf <- if B.null dff || isJust (BC.elemIndex '<' dff)
          then do finishedOneIO k "inventory"
                  have_inventory <- doesRemoteFileExist (repo++"/"++darcsdir++"/inventory")
                  return $
                    case have_inventory of
                        Right _ -> Right defaultRepoFormat
                        Left e -> Left . unlines $
                                    [ "Not a repository: "++repo++" ("++e++")"
                                    , ""
                                    , "HINT: Do you have the right URI for the repository?"
                                    , ""
                                    , "      If so, check with the repository owner to see if the following files"
                                    , "      are readable:"
                                    , ""
                                    , "        1. _darcs/format    - might not exist; that's OK"
                                    , "        2. _darcs/inventory - should exist if #1 is missing"
                                    , "        3. _darcs/hashed_inventory - should exist if #2 is missing"
                                    ]
          else return $ Right $ parseRepoFormat dff
    endTedious k
    return rf
    where drfe x = fetchFilePS x Cachable >> return True
          doesRemoteFileExist x = fmap Right (drfe x) `catchNonSignal`
                                  (\e -> return (Left (prettyException e)))

-- | @writeRepoFormat@ writes the repo format to the given file.
writeRepoFormat :: RepoFormat -> FilePath -> IO ()
writeRepoFormat (RF rf) loc = writeBinFile loc $ unlines $
                              map (BC.unpack . BU.intercalate (BC.singleton '|')) rf

parseRepoFormat :: B.ByteString -> RepoFormat
parseRepoFormat ps =
    RF $ map (BC.split '|') $ filter (not . B.null) $ linesPS ps

-- | The repo format we assume if we do not find a format file.
defaultRepoFormat :: RepoFormat
defaultRepoFormat = RF [[rp2ps Darcs1_0]]

createRepoFormat :: [DarcsFlag] -> RepoFormat
createRepoFormat fs = RF (map rp2ps flags2inv : maybe2)
    where flags2inv | UseFormat2 `elem` fs = [HashedInventory]
                    | UseHashedInventory `elem` fs = [HashedInventory]
                    | UseOldFashionedInventory `elem` fs = [Darcs1_0]
                    | otherwise = [HashedInventory]
          maybe2 = if UseFormat2 `notElem` fs &&
                      (UseOldFashionedInventory `elem` fs ||
                       UseHashedInventory `elem` fs)
                   then []
                   else [[rp2ps Darcs2]]

-- | @writeProblem form@ tells if we can write to a repo in format @form@.
-- It returns @Nothing@ if there's no problem writing to such a repository.
writeProblem :: RepoFormat -> Maybe String
writeProblem rf = readProblem rf `mplus` allProblems rf wp
    where wp x | all isKnown x = Nothing
          wp [] = impossible
          wp x = Just $ unwords $ "Can't write repository format: " :
                 map BC.unpack (filter (not . isKnown) x)


-- | @readfromAndWritetoProblem form@ tells if we can read from and write to a repo in
-- format @form@. It returns @Nothing@ if there's no problem reading
-- and writing to such a repository.
readfromAndWritetoProblem :: RepoFormat -> RepoFormat -> Maybe String
readfromAndWritetoProblem inrf outrf
    | formatHas Darcs2 inrf /= formatHas Darcs2 outrf
        = Just "Cannot mix darcs-2 repositories with older formats"
    | otherwise = readProblem inrf `mplus` writeProblem outrf


-- | @readProblem form@ tells if we can read from a repo in format @form@.
-- It returns @Nothing@ if there's no problem reading from such a repository.
readProblem :: RepoFormat -> Maybe String
readProblem rf | formatHas Darcs1_0 rf && formatHas Darcs2 rf
                    = Just "Invalid repositoryformat:  format 2 is incompatible with format 1"
readProblem rf = allProblems rf rp
    where rp x | any isKnown x = Nothing
          rp [] = impossible
          rp x = Just $ unwords $
                 "Can't understand repository format:" : map BC.unpack x


allProblems :: RepoFormat -> ([B.ByteString] -> Maybe String) -> Maybe String
allProblems (RF ks) repoFormatLineProblem = maybeSingleError $ mapMaybe repoFormatLineProblem ks
    where
        maybeSingleError [] = Nothing
        maybeSingleError xs = Just $ unlines xs


-- | Does this version of darcs know how to handle this property?
isKnown :: B.ByteString -> Bool
isKnown p = p `elem` map rp2ps knownProperties

-- | This is the list of properties which this version of darcs knows
-- how to handle.
knownProperties :: [RepoProperty]
knownProperties = [Darcs1_0, Darcs2, HashedInventory]

formatHas :: RepoProperty -> RepoFormat -> Bool
formatHas f (RF ks) = rp2ps f `elem` concat ks

rp2ps :: RepoProperty -> B.ByteString
rp2ps Darcs1_0 = BC.pack "darcs-1.0"
rp2ps Darcs2 = BC.pack "darcs-2"
rp2ps HashedInventory = BC.pack "hashed"
