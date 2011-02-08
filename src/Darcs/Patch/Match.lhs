%  Copyright (C) 2004 David Roundy
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

#include "gadts.h"

module Darcs.Patch.Match ( PatchMatch, Matcher, MatchFun,
                    patchMatch, matchPattern,
                    applyMatcher, makeMatcher,
                    parseMatch,
                    matchParser, helpOnMatchers,
                  ) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.Regex ( mkRegex, matchRegex )
import Data.Maybe ( isJust )
import System.IO.Unsafe ( unsafePerformIO )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, hopefully, info )
import Darcs.Patch ( Patchy, hunkMatches, listTouchedFiles, patchcontents )
import Darcs.Patch.Info ( justName, justAuthor, justLog, makeFilename,
                          piDate )
import Darcs.Witnesses.Sealed ( Sealed2(..), seal2 )
import DateMatcher ( parseDateMatcher )

import Darcs.Patch.MatchData ( PatchMatch(..), patchMatch )
import qualified Data.ByteString.Char8 as BC

import Darcs.Patch.Dummy ( DummyPatch )

-- | A type for predicates over patches which do not care about
-- contexts
type MatchFun p = Sealed2 (PatchInfoAnd p) -> Bool

-- | A @Matcher@ is made of a 'MatchFun' which we will use to match
-- patches and a @String@ representing it.
data Matcher p = MATCH String (MatchFun p)

instance Show (Matcher p) where
    show (MATCH s _) = '"':s ++ "\""

makeMatcher :: String -> (Sealed2 (PatchInfoAnd p) -> Bool) -> Matcher p
makeMatcher s m = MATCH s m

-- | @applyMatcher@ applies a matcher to a patch.
applyMatcher :: Matcher p -> PatchInfoAnd p C(x y) -> Bool
applyMatcher (MATCH _ m) = m . seal2

parseMatch :: Patchy p => PatchMatch -> Either String (MatchFun p)
parseMatch (PatternMatch s) =
    case parse matchParser "match" s of
    Left err -> Left $ "Invalid -"++"-match pattern '"++s++
                "'.\n"++ unlines (map ("    "++) $ lines $ show err) -- indent
    Right m -> Right m

matchPattern :: Patchy p => PatchMatch -> Matcher p
matchPattern p@(PatternMatch s) =
    case parseMatch p of
    Left err -> error err
    Right m -> makeMatcher s m

trivial :: Patchy p => MatchFun p
trivial = const True
\end{code}

\subsection{Match}

Currently \verb!--match! accepts eight primitive match types, although
there are plans to expand it to match more patterns.  Also, note that the
syntax is still preliminary and subject to change.

The first match type accepts a literal string which is checked against
the patch name.  The syntax is
\begin{verbatim}
darcs annotate --summary --match 'exact foo+bar'
\end{verbatim}
This is useful for situations where a patch name contains characters that
could be considered special for regular expressions.

In this and the other match types, the argument must be enclosed in double
quotes if it contains spaces.  You can escape a quote in the argument with a
backslash; backslash escapes itself, but it is treated literally if followed
by a character other than a double quote or backslash, so it is typically not
necessary to escape a backslash.  No such escaping is necessary unless the
argument is enclosed in double quotes.

The second match type accepts a regular expression which is checked against
the patch name.  The syntax is
\begin{verbatim}
darcs annotate --summary --match 'name foo'
\end{verbatim}
Note that to match regexp metacharacters, such as \verb|(|, literally, they
must be escaped with backslash along with any embedded double quotes.  To
match a literal backslash it must be written quadrupled in general, but often
it need not be escaped, since backslash is only special in regexps when
followed by a metacharacter.  In the following example pairs, the first
literal is matched by the second sequence in the match name:
``\verb|"|'':``\verb|\"|'', ``\verb|\|'':``\verb|\\\\|'',
``\verb|\x|'':``\verb|\x|'', ``\verb|(|'':``\verb|\(|''.

The third match type matches the darcs hash for each patch:
\begin{verbatim}
darcs annotate --summary --match \
  'hash 20040403105958-53a90-c719567e92c3b0ab9eddd5290b705712b8b918ef'
\end{verbatim}
Note you need to provide the full hash string as above.
This is intended to be used, for example, by programs allowing you to view
darcs repositories (e.g.\ CGI scripts like viewCVS).

The fourth match type accepts a regular expression which is checked against
the patch author.  The syntax is
\begin{verbatim}
darcs annotate --summary --match 'author foo'
\end{verbatim}

There is also support for matching by date.  This is done using commands such as
\begin{verbatim}
darcs annotate --summary --match 'date "last week"'
darcs annotate --summary --match 'date yesterday'
darcs annotate --summary --match 'date "today 14:00"'
darcs annotate --summary --match 'date "tea time yesterday"'
darcs annotate --summary --match 'date "3 days before last year at 17:00"'
darcs changes --from-match 'date "Sat Jun  30 11:31:30 EDT 2004"'
\end{verbatim}

Only English date specifications are supported---specifically you must use
English day and month names.  Also, only a limited set of time zones is
supported (compatible with GNU coreutils' date parsing).  Unrecognized zones
are treated as UTC, which may result in the timestamps printed in change
entries not being recognized by the date matching.  You can avoid this problem
on a POSIX-like system by running darcs in the UTC zone to get the times
initially, e.g.:
\begin{verbatim}
TZ=UTC darcs changes
\end{verbatim}

When matching on the ISO format, a partial date is treated as a range.
English dates can either refer to a specific day (``6 months ago',``day before
yesterday''), or to an interval
from some past date (``last month'') to the present.  Putting this all
together, if today is ``2004-07-24'' then the following matches should work:

\begin{tabular}{|ll|}
\hline
\textbf{date} & \textbf{patches selected} \\
\hline
2004          & from 2004-01-01 up to and including 2004-12-31 \\
2004-01       & from 2004-01-01 up to and including 2004-01-31 \\
2004-01-01    & during 2004-01-01 \\
\hline
today         & during 2004-07-24 (starting midnight in your timezone) \\
yesterday     & during 2004-07-23 \\
6 months ago  & during 2004-01-23 \\
\hline
last 6 months & since  2004-01-23 \\
last month    & since  2004-06-23 (not 2004-06-01!) \\
last week     & since  2004-07-16 \\
\hline
\end{tabular}

For more precise control, you may specify an interval, either
in a small subset of English or
of \htmladdnormallinkfoot{the ISO 8601 format}{http://www.w3.org/TR/NOTE-datetime}.
If you use the ISO format, note that durations, when
specified alone, are interpreted as being relative to the current date and time.
\begin{verbatim}
darcs annotate --summary --match 'date "between 2004-03-12 and last week"'
darcs annotate --summary --match 'date "after 2005"'
darcs annotate --summary --match 'date "in the last 3 weeks"'
darcs annotate --summary --match 'date "P3M/2006-03-17"'
darcs annotate --summary --match 'date "2004-01-02/2006-03-17"'
darcs annotate --summary --match 'date "P2M6D"'
\end{verbatim}

You may also prefer to combine date matching with a more specific pattern.
\begin{verbatim}
darcs annotate --summary --match 'date "last week" && name foo'
\end{verbatim}

The sixth match type accepts a regular expression which is checked against
file paths that the patch touches.  The syntax is
\begin{verbatim}
darcs annotate --summary --match 'touch foo/bar.c'
\end{verbatim}

The seventh match type accepts a regular expression which is checked
against every hunk. The syntax is
\begin{verbatim}
darcs annotate --summary --match 'hunk "^instance .* Foo where$"'
\end{verbatim}

The eight match type accepts a regular expression which is checked
against the long comment. The syntax is
\begin{verbatim}
darcs annotate --summary --match 'comment "remote repository"'
\end{verbatim}

The \verb!--match! pattern can include the logical operators \verb!&&!,
\verb!||! and \verb!not!, as well as grouping of patterns with parentheses.
For example
\begin{verbatim}
darcs annotate --summary --match 'name record && not name overrode'
\end{verbatim}

\begin{code}
matchParser :: Patchy p => CharParser st (MatchFun p)
matchParser =  do m <- option trivial submatch
                  eof
                  return m

submatch :: Patchy p => CharParser st (MatchFun p)
submatch = buildExpressionParser table match <?> "match rule"

table :: OperatorTable Char st (MatchFun p)
table   = [ [prefix "not" negate_match,
             prefix "!" negate_match ]
          , [binary "||" or_match,
             binary "or" or_match,
             binary "&&" and_match,
            binary "and" and_match ]
          ]
    where binary name fun =
              Infix (do _ <- trystring name
                        spaces
                        return fun) AssocLeft
          prefix  name fun = Prefix $ do _ <- trystring name
                                         spaces
                                         return fun
          negate_match a p = not (a p)
          or_match m1 m2 p = (m1 p) || (m2 p)
          and_match m1 m2 p = (m1 p) && (m2 p)

trystring :: String -> CharParser st String
trystring s = try $ string s

match :: Patchy p => CharParser st (MatchFun p)
match = between spaces spaces
        (parens submatch
         <|> choice matchers_
         <?> "simple match")
        where matchers_ = map createMatchHelper primitiveMatchers


createMatchHelper :: (String, String, [String], String -> MatchFun p)
                  -> CharParser st (MatchFun p)
createMatchHelper (key,_,_,matcher) =
  do _ <- trystring key
     spaces
     q <- quoted
     return $ matcher q

-- FIXME: would this be better defined in Darcs.Commands.Help?
-- | The string that is emitted when the user runs @darcs help --match@.
helpOnMatchers :: String
helpOnMatchers = unlines $
  ["Selecting Patches:",
   "",
   "The --patches option yields patches with names matching an `extended'",
   "regular expression.  See regex(7) for details.  The --matches option",
   "yields patches that match a logical (Boolean) expression: one or more",
   "primitive expressions combined by grouping (parentheses) and the",
   "complement (not), conjunction (and) and disjunction (or) operators.",
   "The C notation for logic operators (!, && and ||) can also be used.",
   "",
   " --patches=regex is a synonym for --matches='name regex'",
   " --from-patch and --to-patch are synonyms for --from-match='name... and --to-match='name...",
   " --from-patch and --to-match can be unproblematically combined:",
   " darcs changes --from-patch='html.*documentation' --to-match='date 20040212'",
   "",
   "The following primitive Boolean expressions are supported:"]
  ++ keywords
  ++ ["", "Here are some examples:"]
  ++ examples
  where -- This type signature exists to appease GHC.
        ps :: [(String, String, [String], String -> MatchFun DummyPatch)]
        ps = primitiveMatchers
        keywords = [showKeyword k d | (k,d,_,_) <- ps]
        examples = [showExample k e | (k,_,es,_) <- ps, e <- es]
        showKeyword keyword description =
            -- FIXME: it would be nice to have a variable name here:
            -- "author REGEX - match against author (email address)"
            -- or "exact STRING - match against exact patch name".
            "  " ++ keyword ++ " - " ++ description ++ "."
        showExample keyword example =
            -- FIXME: this string is long, and its not a use case I've
            -- ever seen in practice.  Can we use something else,
            -- like "darcs changes --matches"? --twb, 2008-12-28
            "  darcs annotate --summary --match "
            ++ "'" ++ keyword ++ " " ++ example ++ "'"

primitiveMatchers :: Patchy p => [(String, String, [String], String -> MatchFun p)]
                     -- ^ keyword (operator), help description, list
                     -- of examples, matcher function
primitiveMatchers =
 [ ("exact", "check a literal string against the patch name"
           , ["\"Resolve issue17: use dynamic memory allocation.\""]
           , exactmatch )
 , ("name", "check a regular expression against the patch name"
          , ["issue17", "\"^[Rr]esolve issue17\\>\""]
          , mymatch )
 , ("author", "check a regular expression against the author name"
            , ["\"David Roundy\"", "droundy", "droundy@darcs.net"]
            , authormatch )
 , ("hunk", "check a regular expression against the contents of a hunk patch"
            , ["\"foo = 2\"", "\"^instance .* Foo where$\""]
            , hunkmatch )
 , ("comment", "check a regular expression against the log message"
         , ["\"prevent deadlocks\""]
         , logmatch )
 , ("hash",  "match the darcs hash for a patch"
          ,  ["20040403105958-53a90-c719567e92c3b0ab9eddd5290b705712b8b918ef"]
          ,  hashmatch )
 , ("date", "match the patch date"
          , ["\"2006-04-02 22:41\"", "\"tea time yesterday\""]
          , datematch )
 , ("touch", "match file paths for a patch"
          , ["src/foo.c", "src/", "\"src/*.(c|h)\""]
          , touchmatch ) ]

parens :: CharParser st (MatchFun p)
       -> CharParser st (MatchFun p)
parens p  = between (string "(") (string ")") p

quoted :: CharParser st String
quoted = between (char '"') (char '"')
                 (many $ do { _ <- char '\\' -- allow escapes
                            ; try (oneOf ['\\', '"']) <|> return '\\'
                            }
                         <|>  noneOf ['"'])
         <|> between spaces spaces (many $ noneOf " ()")
         <?> "string"

mymatch, exactmatch, authormatch, hunkmatch, hashmatch, datematch, touchmatch :: Patchy p => String -> MatchFun p

mymatch r (Sealed2 hp) = isJust $ matchRegex (mkRegex r) $ justName (info hp)

exactmatch r (Sealed2 hp) = r == (justName (info hp))

authormatch a (Sealed2 hp) = isJust $ matchRegex (mkRegex a) $ justAuthor (info hp)

logmatch :: Patchy p => String -> MatchFun p
logmatch l (Sealed2 hp) = isJust $ matchRegex (mkRegex l) $ justLog (info hp)

hunkmatch r (Sealed2 hp) = let patch = patchcontents $ hopefully hp
                               regexMatcher = isJust . (matchRegex (mkRegex r) . BC.unpack)
                           in hunkMatches regexMatcher patch

hashmatch h (Sealed2 hp) = let rh = makeFilename (info hp) in
                                  (rh == h) || (rh == h++".gz")

datematch d (Sealed2 hp) = let dm = unsafePerformIO $ parseDateMatcher d
                                  in dm $ piDate (info hp)

touchmatch r (Sealed2 hp) = let files = listTouchedFiles $ patchcontents $ hopefully hp
                            in or $ map (isJust . matchRegex (mkRegex r)) files
\end{code}
