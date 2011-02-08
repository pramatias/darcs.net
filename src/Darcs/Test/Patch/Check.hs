-- Copyright (C) 2002-2003 David Roundy
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; see the file COPYING.  If not, write to
-- the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301, USA.

module Darcs.Test.Patch.Check ( PatchCheck(), doCheck, fileExists, dirExists,
                                removeFile, removeDir, createFile, createDir,
                                insertLine, deleteLine, isValid, doVerboseCheck,
                                fileEmpty,
                                checkMove, modifyFile, FileContents(..)
                              ) where

import System.IO.Unsafe ( unsafePerformIO )
import qualified Data.ByteString as B (ByteString)
import Data.List ( isPrefixOf, inits )
import Control.Monad.State ( State, evalState, runState )
import Control.Monad.State.Class ( get, put, modify )
-- use Map, not IntMap, because Map has mapKeys and IntMap hasn't
import Data.Map ( Map )
import qualified Data.Map as M ( mapKeys, delete, insert, empty, lookup, null )
import System.FilePath ( joinPath, splitDirectories )

-- | File contents are represented by a map from line numbers to line contents.
--   If for a certain line number, the line contents are Nothing, that means
--   that we are sure that that line exists, but we don't know its contents.
--   We must also store the greatest line number that is known to exist in a
--   file, to be able to exclude the possibility of it being empty without
--   knowing its contents.
data FileContents = FC { fcLines   :: Map Int B.ByteString
                       , fcMaxline :: Int
                       } deriving (Eq, Show)
data Prop = FileEx String | DirEx String | NotEx String
          | FileLines String FileContents
            deriving (Eq)
-- | A @KnownState@ is a simulated repository state. The repository is either
-- inconsistent, or it has two lists of properties: one list with properties
-- that hold for this repo, and one with properties that do not hold for this
-- repo. These two lists may not have any common elements: if they had, the
-- repository would be inconsistent.
data KnownState = P [Prop] [Prop]
                | Inconsistent
                  deriving (Show)
instance  Show Prop  where
    show (FileEx f) = "FileEx "++f
    show (DirEx d)  = "DirEx "++d
    show (NotEx f) = "NotEx"++f
    show (FileLines f l)  = "FileLines "++f++": "++show l

-- TODO the way that the standard way to use PatchCheck is
-- by returning PatchCheck Bool but then often ignoring the
-- result and instead checking again for state consistency
-- is weird. It should be possible to replace it by a more normal
-- error handling mechanism.

-- | PatchCheck is a state monad with a simulated repository state
type PatchCheck = State KnownState

-- | The @FileContents@ structure for an empty file
emptyFilecontents :: FileContents
emptyFilecontents = FC M.empty 0

-- | Returns a given value if the repository state is inconsistent, and performs
--   a given action otherwise.
handleInconsistent :: a            -- ^ The value to return if the state is inconsistent
                   -> PatchCheck a -- ^ The action to perform otherwise
                   -> PatchCheck a
handleInconsistent v a =  do state <- get
                             case state of
                               Inconsistent -> return v
                               _            -> a

doCheck :: PatchCheck a -> a
doCheck p = evalState p (P [] [])

-- | Run a check, and print the final repository state
doVerboseCheck :: PatchCheck a -> a
doVerboseCheck p =
    case runState p (P [] []) of
    (b, pc) -> unsafePerformIO $ do putStrLn $ show pc
                                    return b

-- | Returns true if the current repository state is not inconsistent
isValid :: PatchCheck Bool
isValid = handleInconsistent False (return True)

has :: Prop -> [Prop] -> Bool
has _ [] = False
has k (k':ks) = k == k' || has k ks

modifyFile :: String
            -> (Maybe FileContents -> Maybe FileContents)
            -> PatchCheck Bool
modifyFile f change = do
    _ <- fileExists f
    c <- fileContents f
    case change c of
      Nothing -> assertNot $ FileEx f -- shorthand for "FAIL"
      Just c' -> do setContents f c'
                    isValid

insertLine :: String -> Int -> B.ByteString -> PatchCheck Bool
insertLine f n l = do
    c <- fileContents f
    case c of
      Nothing -> assertNot $ FileEx f -- in this case, the repo is inconsistent
      Just c' -> do
        let lines'   = M.mapKeys (\k -> if k >= n then k+1 else k) (fcLines c')
            lines''  = M.insert n l lines'
            maxline' = max n (fcMaxline c')
        setContents f (FC lines'' maxline')
        return True

-- deletes a line from a hunk patch (third argument) in the given file (first
-- argument) at the given line number (second argument)
deleteLine :: String -> Int -> B.ByteString -> PatchCheck Bool
deleteLine f n l = do
    c <- fileContents f
    case c of
      Nothing -> assertNot $ FileEx f
      Just c' ->
        let flines  = fcLines c'
            flines' = M.mapKeys (\k -> if k > n then k-1 else k)
                                (M.delete n flines)
            maxlinenum' | n <= fcMaxline c'  = fcMaxline c' - 1
                        | otherwise           = n - 1
            c'' = FC flines' maxlinenum'
            do_delete = do
              setContents f c''
              isValid
        in case M.lookup n flines of
          Nothing -> do_delete
          Just l' -> if l == l'
                       then do_delete
                       else assertNot $ FileEx f

setContents :: String -> FileContents -> PatchCheck ()
setContents f c = handleInconsistent () $ do
    P ks nots <- get
    let ks' = FileLines f c : filter (not . is_file_lines_for f) ks
    put (P ks' nots)
  where is_file_lines_for file prop = case prop of
                                        FileLines f' _ -> file == f'
                                        _              -> False

-- | Get (as much as we know about) the contents of a file in the current state.
--   Returns Nothing if the state is inconsistent.
fileContents :: String -> PatchCheck (Maybe FileContents)
fileContents f = handleInconsistent Nothing $ do
      P ks _ <- get
      return (fic ks)
    where fic (FileLines f' c:_) | f == f' = Just c
          fic (_:ks) = fic ks
          fic [] = Just emptyFilecontents

-- | Checks if a file is empty
fileEmpty :: String          -- ^ Name of the file to check
           -> PatchCheck Bool
fileEmpty f = do
  c <- fileContents f
  let empty = case c of
               Just c' -> fcMaxline c' == 0 && M.null (fcLines c')
               Nothing -> True
  _ <- if empty
     then do setContents f emptyFilecontents
             isValid
     -- Crude way to make it inconsistent and return false:
     else assertNot $ FileEx f
  return empty

movedirfilename :: String -> String -> String -> String
movedirfilename d d' f
  | (d ++ "/") `isPrefixOf` f = d' ++ drop (length d) f
  | f == d = d'
  | otherwise = f

-- | Replaces a filename by another in all paths. Returns True if the repository
--   is consistent, False if it is not.
doSwap :: String -> String -> PatchCheck Bool
doSwap f f' = handleInconsistent False $ do
    modify (\(P ks nots) -> P (map sw ks) (map sw nots))
    return True
  where sw (FileEx a) | f  `is_soe` a = FileEx $ movedirfilename f f' a
                      | f' `is_soe` a = FileEx $ movedirfilename f' f a
        sw (DirEx a) | f  `is_soe` a = DirEx $ movedirfilename f f' a
                     | f' `is_soe` a = DirEx $ movedirfilename f' f a
        sw (FileLines a c) | f  `is_soe` a = FileLines (movedirfilename f f' a) c
                           | f' `is_soe` a = FileLines (movedirfilename f' f a) c
        sw (NotEx a) | f `is_soe` a = NotEx $ movedirfilename f f' a
                     | f' `is_soe` a = NotEx $ movedirfilename f' f a
        sw p = p
        is_soe d1 d2 = -- is_superdir_or_equal
            d1 == d2 || (d1 ++ "/") `isPrefixOf` d2

-- | Assert a property about the repository. If the property is already present
-- in the repo state, nothing changes, and the function returns True. If it is
-- not present yet, it is added to the repo state, and the function is True. If
-- the property is already in the list of properties that do not hold for the
-- repo, the state becomes inconsistent, and the function returns false.
assert :: Prop -> PatchCheck Bool
assert p = handleInconsistent False $ do
    P ks nots <- get
    if has p nots
      then do
        put Inconsistent
        return False
      else if has p ks
             then return True
             else do
               put (P (p:ks) nots)
               return True

-- | Like @assert@, but negatively: state that some property must not hold for
--   the current repo.
assertNot :: Prop -> PatchCheck Bool
assertNot p = handleInconsistent False $ do
    P ks nots <- get
    if has p ks
      then do
        put Inconsistent
        return False
      else if has p nots
             then return True
             else do
               put (P ks (p:nots))
               return True

-- | Remove a property from the list of properties that do not hold for this
-- repo (if it's there), and add it to the list of properties that hold.
-- Returns False if the repo is inconsistent, True otherwise.
changeToTrue :: Prop -> PatchCheck Bool
changeToTrue p = handleInconsistent False $ do
    modify (\(P ks nots) -> P (p:ks) (filter (p /=) nots))
    return True

-- | Remove a property from the list of properties that hold for this repo (if
-- it's in there), and add it to the list of properties that do not hold.
-- Returns False if the repo is inconsistent, True otherwise.
changeToFalse :: Prop -> PatchCheck Bool
changeToFalse p = handleInconsistent False $ do
    modify (\(P ks nots) -> P (filter (p /=) ks) (p:nots))
    return True

assertFileExists :: String -> PatchCheck Bool
assertFileExists f =   do _ <- assertNot $ NotEx f
                          _ <- assertNot $ DirEx f
                          assert $ FileEx f
assertDirExists :: String -> PatchCheck Bool
assertDirExists d =   do _ <- assertNot $ NotEx d
                         _ <- assertNot $ FileEx d
                         assert $ DirEx d
assertExists :: String -> PatchCheck Bool
assertExists f = assertNot $ NotEx f

assertNoSuch :: String -> PatchCheck Bool
assertNoSuch f =   do _ <- assertNot $ FileEx f
                      _ <- assertNot $ DirEx f
                      assert $ NotEx f

createFile :: String -> PatchCheck Bool
createFile fn = do
  _ <- superdirsExist fn
  _ <- assertNoSuch fn
  _ <- changeToTrue (FileEx fn)
  changeToFalse (NotEx fn)

createDir :: String -> PatchCheck Bool
createDir fn = do
  _ <- substuffDontExist fn
  _ <- superdirsExist fn
  _ <- assertNoSuch fn
  _ <- changeToTrue (DirEx fn)
  changeToFalse (NotEx fn)

removeFile :: String -> PatchCheck Bool
removeFile fn = do
  _ <- superdirsExist fn
  _ <- assertFileExists fn
  _ <- fileEmpty fn
  _ <- changeToFalse (FileEx fn)
  changeToTrue (NotEx fn)

removeDir :: String -> PatchCheck Bool
removeDir fn = do
  _ <- substuffDontExist fn
  _ <- superdirsExist fn
  _ <- assertDirExists fn
  _ <- changeToFalse (DirEx fn)
  changeToTrue (NotEx fn)

checkMove :: String -> String -> PatchCheck Bool
checkMove f f' = do
  _ <- superdirsExist f
  _ <- superdirsExist f'
  _ <- assertExists f
  _ <- assertNoSuch f'
  doSwap f f'

substuffDontExist :: String -> PatchCheck Bool
substuffDontExist d = handleInconsistent False $ do
    P ks _ <- get
    if all noss ks
      then return True
      else do
        put Inconsistent
        return False
  where noss (FileEx f) = not (is_within_dir f)
        noss (DirEx f) = not (is_within_dir f)
        noss _ = True
        is_within_dir f = (d ++ "/") `isPrefixOf` f

-- the init and tail calls dump the final init (which is just the path itself
-- again), the first init (which is empty), and the initial "." from
-- splitDirectories
superdirsExist :: String -> PatchCheck Bool
superdirsExist fn = and `fmap` mapM assertDirExists superdirs
  where superdirs =  map (("./"++) . joinPath)
                         (init (tail (inits (tail (splitDirectories fn)))))

fileExists :: String -> PatchCheck Bool
fileExists fn = do
  _ <- superdirsExist fn
  assertFileExists fn

dirExists :: String -> PatchCheck Bool
dirExists fn = do
  _ <- superdirsExist fn
  assertDirExists fn
