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

{-# LANGUAGE CPP #-}

module Darcs.Patch.Read ( ReadPatch(..),
                          readPatch, readPatchPartial,
                          bracketedFL, peekfor,
                          readFileName )
             where

import Prelude hiding ( pi )

import ByteStringUtils ( dropSpace )
import qualified Data.ByteString       as B  (ByteString, null)

import Darcs.Patch.Bracketed ( Bracketed(..), unBracketedFL )
import Darcs.Patch.FileName ( FileName, fp2fn, ps2fn, decodeWhite )
import Darcs.Patch.Format ( PatchListFormat(..), ListFormat(..), FileNameFormat(..) )
import Darcs.Patch.ReadMonads (ParserM,
                               parseStrictly,
                               choice, lexChar, lexString,
                               checkConsumes )

import Darcs.Witnesses.Ordered ( FL(..), RL, reverseFL )
import Darcs.Witnesses.Sealed ( Sealed(..), mapSeal )

import Control.Applicative ( (<$>), (<|>) )
import Control.Monad ( mzero )
import qualified Data.ByteString.Char8 as BC ( ByteString, unpack )

#include "gadts.h"

class ReadPatch p where
    readPatch'
        :: ParserM m => m (Sealed (p C(x )))

readPatchPartial :: ReadPatch p => B.ByteString -> Maybe (Sealed (p C(x )), B.ByteString)
readPatchPartial ps
    = case parseStrictly readPatch' ps of
         Just (p, ps') -> Just (p, ps')
         _ -> Nothing

readPatch :: ReadPatch p => B.ByteString -> Maybe (Sealed (p C(x )))
readPatch ps
    = case readPatchPartial ps of
         Just (p, ps') | B.null (dropSpace ps') -> Just p
         _ -> Nothing

instance ReadPatch p => ReadPatch (Bracketed p) where
    readPatch' = mapSeal Braced <$> bracketedFL readPatch' '{' '}'
                   <|>
                 mapSeal Parens <$> bracketedFL readPatch' '(' ')'
                   <|>
                 mapSeal Singleton <$> readPatch'

instance (ReadPatch p, PatchListFormat p) => ReadPatch (FL p) where
    readPatch'
        | ListFormatV1 <- patchListFormat :: ListFormat p
            = mapSeal unBracketedFL <$> readPatch'
        -- in the V2 format case, we only need to support () on reading, not {}
        -- for simplicity we just go through the same code path.
        | ListFormatV2 <- patchListFormat :: ListFormat p
            = mapSeal unBracketedFL <$> readPatch'
        | otherwise
            = read_patches
     where read_patches :: ParserM m => m (Sealed (FL p C(x )))
           read_patches = do --tracePeek "starting FL read"
                             -- checkConsumes is needed to make sure that something is read,
                             -- to avoid stack overflow when parsing FL (FL p)
                             mp <- (Just <$> checkConsumes readPatch') <|> return Nothing
                             case mp of
                               Just (Sealed p) -> do --tracePeek "found one patch"
                                                     Sealed ps <- read_patches
                                                     return $ Sealed (p:>:ps)
                               Nothing -> return $ Sealed NilFL
--           tracePeek x = do y <- peekInput
--                            traceDoc (greenText x $$ greenText (show $ sal_to_string y)) return ()

instance (ReadPatch p, PatchListFormat p) => ReadPatch (RL p) where
    readPatch' = mapSeal reverseFL <$> readPatch'

{-# INLINE bracketedFL #-}
bracketedFL :: forall p m C(x) . (ParserM m) =>
               (FORALL(y) m (Sealed (p C(y)))) -> Char -> Char -> m (Sealed (FL p C(x)))
bracketedFL parser pre post =
    peekforc pre bfl mzero
        where bfl :: FORALL(z) m (Sealed (FL p C(z)))
              bfl = peekforc post (return $ Sealed NilFL)
                                  (do Sealed p <- parser
                                      Sealed ps <- bfl
                                      return $ Sealed (p:>:ps))

{-# INLINE peekforc #-}
peekforc :: ParserM m => Char -> m a -> m a -> m a
peekforc c ifstr ifnot = choice [ lexChar c >> ifstr
                                , ifnot ]

peekfor :: ParserM m => BC.ByteString -> m a -> m a -> m a
peekfor ps ifstr ifnot = choice [ do lexString ps
                                     ifstr
                                , ifnot ]
{-# INLINE peekfor #-}

readFileName :: FileNameFormat -> B.ByteString -> FileName
readFileName OldFormat = ps2fn
readFileName NewFormat = fp2fn . decodeWhite . BC.unpack
