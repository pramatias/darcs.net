{-# OPTIONS_GHC -fno-warn-orphans #-}
module Darcs.Patch.Prim.V1.Read () where

import Darcs.Patch.Prim.Class ( PrimRead(..), hunk, binary )
import Darcs.Patch.Prim.V1.Core
    ( Prim(..),
      DirPatchType(..), FilePatchType(..) )

import Darcs.Patch.FileName ( fn2fp )
import Darcs.Patch.Format ( FileNameFormat(..) )
import Darcs.Patch.Read ( ReadPatch(..), readFileName )
import Darcs.Patch.ReadMonads (ParserM, takeTillChar,
                               string, int,
                               option, choice,
                               anyChar, char, myLex',
                               skipSpace, skipWhile, linesStartingWith)

import Darcs.Witnesses.Sealed ( seal )

import ByteStringUtils ( fromHex2PS )

import Control.Monad ( liftM )
import qualified Data.ByteString       as B  ( ByteString, init, tail, concat )
import qualified Data.ByteString.Char8 as BC ( unpack, pack )

#include "gadts.h"

instance ReadPatch Prim where
 readPatch' = readPrim OldFormat

instance PrimRead Prim where
  readPrim x
     = skipSpace >> choice
       [ return' $ readHunk x
       , return' $ readAddFile x
       , return' $ readAddDir x
       , return' $ readMove x
       , return' $ readRmFile x
       , return' $ readRmDir x
       , return' $ readTok x
       , return' $ readBinary x
       , return' $ readChangePref
       ]
    where
    return'  = liftM seal

hunk' :: B.ByteString
hunk' = BC.pack "hunk"

replace :: B.ByteString
replace = BC.pack "replace"

binary' :: B.ByteString
binary' = BC.pack "binary"

addfile :: B.ByteString
addfile = BC.pack "addfile"

adddir :: B.ByteString
adddir = BC.pack "adddir"

rmfile :: B.ByteString
rmfile = BC.pack "rmfile"

rmdir :: B.ByteString
rmdir = BC.pack "rmdir"

move :: B.ByteString
move = BC.pack "move"

changepref :: B.ByteString
changepref = BC.pack "changepref"

readHunk :: ParserM m => FileNameFormat -> m (Prim C(x y))
readHunk x = do
  string hunk'
  fi <- myLex'
  l <- int
  have_nl <- skipNewline
  if have_nl
     then do _ <- linesStartingWith ' ' -- skipping context
             old <- linesStartingWith '-'
             new <- linesStartingWith '+'
             _ <- linesStartingWith ' ' -- skipping context
             return $ hunk (fn2fp $ readFileName x fi) l old new
     else return $ hunk (fn2fp $ readFileName x fi) l [] []

skipNewline :: ParserM m => m Bool
skipNewline = option False (char '\n' >> return True)

readTok :: ParserM m => FileNameFormat -> m (Prim C(x y))
readTok x = do
  string replace
  f <- myLex'
  regstr <- myLex'
  o <- myLex'
  n <- myLex'
  return $ FP (readFileName x f) $ TokReplace (BC.unpack (drop_brackets regstr))
                          (BC.unpack o) (BC.unpack n)
    where drop_brackets = B.init . B.tail


-- * Binary file modification
--
-- | Modify a binary file
--
-- > binary FILENAME
-- > oldhex
-- > *HEXHEXHEX
-- > ...
-- > newhex
-- > *HEXHEXHEX
-- > ...
readBinary :: ParserM m => FileNameFormat -> m (Prim C(x y))
readBinary x = do
  string binary'
  fi <- myLex'
  _ <- myLex'
  skipSpace
  old <- linesStartingWith '*'
  _ <- myLex'
  skipSpace
  new <- linesStartingWith '*'
  return $ binary (fn2fp $ readFileName x fi)
                  (fromHex2PS $ B.concat old)
                  (fromHex2PS $ B.concat new)

readAddFile :: ParserM m => FileNameFormat -> m (Prim C(x y))
readAddFile x = do string addfile
                   f <- myLex'
                   return $ FP (readFileName x f) AddFile

readRmFile :: ParserM m => FileNameFormat -> m (Prim C(x y))
readRmFile x = do string rmfile
                  f <- myLex'
                  return $ FP (readFileName x f) RmFile

readMove :: ParserM m => FileNameFormat -> m (Prim C(x y))
readMove x = do string move
                d <- myLex'
                d' <- myLex'
                return $ Move (readFileName x d) (readFileName x d')

readChangePref :: ParserM m => m (Prim C(x y))
readChangePref
 = do string changepref
      p <- myLex'
      skipWhile (== ' ')
      _ <- anyChar -- skip newline
      f <- takeTillChar '\n'
      _ <- anyChar -- skip newline
      t <- takeTillChar '\n'
      return $ ChangePref (BC.unpack p) (BC.unpack f) (BC.unpack t)

readAddDir :: ParserM m => FileNameFormat -> m (Prim C(x y))
readAddDir x = do string adddir
                  f <- myLex'
                  return $ DP (readFileName x f) AddDir

readRmDir :: ParserM m => FileNameFormat -> m (Prim C(x y))
readRmDir x = do string rmdir
                 f <- myLex'
                 return $ DP (readFileName x f) RmDir

