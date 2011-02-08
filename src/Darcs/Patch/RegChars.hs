-- Copyright (C) 2003 David Roundy
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


module Darcs.Patch.RegChars ( regChars,
                ) where

(&&&) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(&&&) a b c = a c && b c

(|||) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(|||) a b c = a c || b c

{-# INLINE regChars #-}

-- | 'regChars' returns a filter function that tells if a char is a member
-- of the regChar expression or not. The regChar expression is basically a
-- set of chars, but it can contain ranges with use of the '-' (dash), and
-- it can also be specified as a complement set by prefixing with '^'
-- (caret). The dash and caret, as well as the backslash, can all be
-- escaped with a backslash to suppress their special meaning.
--
-- NOTE: The '.' (dot) is allowed to be escaped. It has no special meaning
-- if it is not escaped, but the default 'filename_toks' in
-- Darcs.Commands.Replace uses an escaped dot (WHY?).

regChars :: String -> (Char -> Bool)
regChars ('^':cs) = not . normalRegChars (unescapeChars cs)
regChars ('\\':'^':cs) = normalRegChars $ unescapeChars $ '^':cs
regChars cs = normalRegChars $ unescapeChars cs

{-# INLINE unescapeChars #-}

-- | 'unescapeChars' unescapes whitespace, which is escaped in the replace
-- patch file format. It will also unescape escaped carets, which is useful
-- for escaping a leading caret that should not invert the regChars. All
-- other escapes are left for the unescaping in 'normalRegChars'.

unescapeChars :: String -> String
unescapeChars ('\\':'n':cs) = '\n' : unescapeChars cs
unescapeChars ('\\':'t':cs) = '\t' : unescapeChars cs
unescapeChars ('\\':'^':cs) = '^' : unescapeChars cs
unescapeChars (c:cs) = c : unescapeChars cs
unescapeChars [] = []

{-# INLINE normalRegChars #-}

-- | 'normalRegChars' assembles the filter function. It handles special
-- chars, and also unescaping of escaped special chars. If a non-special
-- char is still escaped by now we get a failure.

normalRegChars :: String -> (Char -> Bool)
normalRegChars ('\\':'.':cs) = (=='.') ||| normalRegChars cs
normalRegChars ('\\':'-':cs) = (=='-') ||| normalRegChars cs
normalRegChars ('\\':'\\':cs) = (=='\\') ||| normalRegChars cs
normalRegChars ('\\':c:_) = error $ "'\\"++[c]++"' not supported."
normalRegChars (c1:'-':c2:cs) = ((>= c1) &&& (<= c2)) ||| normalRegChars cs
normalRegChars (c:cs) = (== c) ||| normalRegChars cs
normalRegChars [] = \_ -> False


