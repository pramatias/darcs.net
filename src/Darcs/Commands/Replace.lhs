%  Copyright (C) 2002-2005 David Roundy
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

\darcsCommand{replace}
\begin{code}
{-# LANGUAGE CPP #-}

module Darcs.Commands.Replace ( replace ) where

import Data.Maybe ( isJust )
import Control.Monad ( unless, filterM )
import Control.Applicative( (<$>) )

import Darcs.Commands ( DarcsCommand(..),
                        nodefaults )
import Darcs.Arguments ( DarcsFlag(ForceReplace, Toks), listRegisteredFiles,
                         ignoretimes, umaskOption, tokens, forceReplace,
                         workingRepoDir, fixSubPaths )
import Darcs.Repository ( withRepoLock, RepoJob(..),
                    addToPending,
                    amInRepository,
                    applyToWorking,
                    readUnrecorded, readRecordedAndPending
                  )
import Darcs.Patch ( Patchy, PrimPatch, tokreplace, forceTokReplace, applyToTree )
import Darcs.Patch.FileName( fn2fp )
import Darcs.Patch.Patchy ( Apply )
import Darcs.Witnesses.Ordered ( FL(..), (+>+), concatFL, toFL )
import Darcs.Witnesses.Sealed ( Sealed(..), mapSeal, FreeLeft, Gap(..) )
import Darcs.Patch.RegChars ( regChars )
import Data.Char ( isSpace )
import Darcs.RepoPath ( SubPath, toFilePath, sp2fn )
import Darcs.Repository.Prefs ( FileType(TextFile) )
import Darcs.Diff( treeDiff )
import Storage.Hashed.Tree( readBlob, modifyTree , findFile, TreeItem(..), Tree, makeBlobBS )
import Storage.Hashed.AnchoredPath( AnchoredPath, floatPath )

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
#include "impossible.h"
#include "gadts.h"

replaceDescription :: String
replaceDescription = "Substitute one word for another."

replaceHelp :: String
replaceHelp =
 "In addition to line-based patches, Darcs supports a limited form of\n" ++
 "lexical substitution.  Files are treated as sequences of words, and\n" ++
 "each occurrence of the old word is replaced by the new word.\n" ++
 "This is intended to provide a clean way to rename a function or\n" ++
 "variable.  Such renamings typically affect lines all through the\n" ++
 "source code, so a traditional line-based patch would be very likely to\n" ++
 "conflict with other branches, requiring manual merging.\n" ++
 "\n" ++
 "Files are tokenized according to one simple rule: words are strings of\n" ++
 "valid token characters, and everything between them (punctuation and\n" ++
 -- FIXME: this heuristic is ham-fisted and silly.  Can we drop it?
 "whitespace) is discarded.  By default, valid token characters are\n" ++
 "letters, numbers and the underscore (i.e. [A-Za-z0-9_]).  However if\n" ++
 "the old and/or new token contains either a hyphen or period, BOTH\n" ++
 "hyphen and period are treated as valid (i.e. [A-Za-z0-9_.-]).\n" ++
 "\n" ++
 "The set of valid characters can be customized using the --token-chars\n" ++
 "option.  The argument must be surrounded by square brackets.  If a\n" ++
 "hyphen occurs between two characters in the set, it is treated as a\n" ++
 "set range.  For example, in most locales [A-Z] denotes all uppercase\n" ++
 "letters.  If the first character is a caret, valid tokens are taken to\n" ++
 "be the complement of the remaining characters.  For example, [^:\\n]\n" ++
 "could be used to match fields in the passwd(5), where records and\n" ++
 "fields are separated by newlines and colons respectively.\n" ++
 "\n" ++
 "If you choose to use --token-chars, you are STRONGLY encouraged to do\n" ++
 "so consistently.  The consequences of using multiple replace patches\n" ++
 "with different --token-chars arguments on the same file are not well\n" ++
 "tested nor well understood.\n" ++
 "\n" ++
 "By default Darcs will refuse to perform a replacement if the new token\n" ++
 "is already in use, because the replacements would be not be\n" ++
 "distinguishable from the existing tokens.  This behaviour can be\n" ++
 "overridden by supplying the --force option, but an attempt to `darcs\n" ++
 "rollback' the resulting patch will affect these existing tokens.\n" ++
 "\n" ++
 "Limitations:\n" ++
 "\n" ++
 "The tokenizer treats files as byte strings, so it is not possible for\n" ++
 "--token-chars to include multi-byte characters, such as the non-ASCII\n" ++
 "parts of UTF-8.  Similarly, trying to replace a `high-bit' character\n" ++
 "from a unibyte encoding will also result in replacement of the same\n" ++
 "byte in files with different encodings.  For example, an acute a from\n" ++
 "ISO 8859-1 will also match an alpha from ISO 8859-7.\n" ++
 "\n" ++
 "Due to limitations in the patch file format, --token-chars arguments\n" ++
 "cannot contain literal whitespace.  For example, [^ \\n\\t] cannot be\n" ++
 "used to declare all characters except the space, tab and newline as\n" ++
 "valid within a word, because it contains a literal space.\n" ++
 "\n" ++
 "Unlike POSIX regex(7) bracket expressions, character classes (such as\n" ++
 "[[:alnum:]]) are NOT supported by --token-chars, and will be silently\n" ++
 "treated as a simple set of characters.\n"

-- FIXME: can  we just  delete the remaining  text?  It seems  more an
-- instance of "look how clever  I am; I made commutation work" rather
-- than information that is actually useful to users.
\end{code}
There is a potentially confusing difference, however, when a replace is
used to make another replace possible:
\begin{verbatim}
$ darcs replace newtoken aaack ./foo.c
$ darcs replace oldtoken newtoken ./foo.c
$ darcs record
\end{verbatim}
will be valid, even if \verb!newtoken! and \verb!oldtoken! are both present
in the recorded version of foo.c, while the sequence
\begin{verbatim}
$ [manually edit foo.c replacing newtoken with aaack]
$ darcs replace oldtoken newtoken ./foo.c
\end{verbatim}
will fail because ``newtoken'' still exists in the recorded version of
\verb!foo.c!.  The reason for the difference is that when recording, a
``replace'' patch always is recorded \emph{before} any manual changes,
which is usually what you want, since often you will introduce new
occurrences of the ``newtoken'' in your manual changes.  In contrast,
multiple ``replace'' changes are recorded in the order in which
they were made.
\begin{code}

replace :: DarcsCommand
replace = DarcsCommand {commandProgramName = "darcs",
                        commandName = "replace",
                        commandHelp = replaceHelp,
                        commandDescription = replaceDescription,
                        commandExtraArgs = -1,
                        commandExtraArgHelp = ["<OLD>","<NEW>",
                                                  "<FILE> ..."],
                        commandCommand = replaceCmd,
                        commandPrereq = amInRepository,
                        commandGetArgPossibilities = listRegisteredFiles,
                        commandArgdefaults = nodefaults,
                        commandAdvancedOptions = [ignoretimes, umaskOption],
                        commandBasicOptions =
                            [tokens, forceReplace, workingRepoDir]}

replaceCmd :: [DarcsFlag] -> [String] -> IO ()
replaceCmd opts (old:new:relfs) = withRepoLock opts $ RepoJob $ \repository -> do
  fs <- fixSubPaths opts relfs
  toks <- chooseToks opts old new
  let checkToken tok =
        unless (isTok toks tok) $ fail $ "'"++tok++"' is not a valid token!"
  checkToken old
  checkToken new
  work <- readUnrecorded repository Nothing
  cur <- readRecordedAndPending repository
  files <- filterM (exists work) fs
  Sealed pswork <- mapSeal concatFL . toFL <$> mapM (repl toks cur work) files
  addToPending repository pswork
  _ <- applyToWorking repository opts pswork `catch` \e ->
      fail $ "Can't do replace on working!\n"
          ++ "Perhaps one of the files already contains '"++ new++"'?\n"
          ++ show e
  return ()
  where ftf _ = TextFile
        skipmsg f = "Skipping file '" ++ toFilePath f ++ "' which isn't in the repository."
        exists tree file = if isJust $ findFile tree (floatSubPath file)
                              then return True
                              else do putStrLn $ skipmsg file
                                      return False
        repl :: forall prim . (Patchy prim, PrimPatch prim) => String -> Tree IO -> Tree IO -> SubPath -> IO (FreeLeft (FL prim))
        repl toks cur work f =
          do work_replaced <- maybeApplyToTree replace_patch work
             cur_replaced <- maybeApplyToTree replace_patch cur
             if ForceReplace `elem` opts || isJust work_replaced || isJust cur_replaced
                then get_force_replace f toks work
                else do putStrLn $ "Skipping file '"++f_fp++"'"
                        putStrLn $ "Perhaps the recorded version of this " ++
                                   "file already contains '" ++new++"'?"
                        putStrLn $ "Use the --force option to override."
                        return (emptyGap NilFL)
          where f_fp = toFilePath f
                replace_patch :: prim C(x y)
                replace_patch = tokreplace f_fp toks old new

        get_force_replace :: PrimPatch prim => SubPath -> String -> Tree IO -> IO (FreeLeft (FL prim))
        get_force_replace f toks tree = do
            let path = floatSubPath f
            content <- readBlob $ fromJust $ findFile tree path
            let newcontent = forceTokReplace toks new old (BS.concat $ BL.toChunks content)
                tree' = modifyTree tree path (File . makeBlobBS <$> newcontent)
            case newcontent of
              Nothing -> bug "weird forcing bug in replace."
              Just _ -> do pfix <- treeDiff ftf tree tree'
                           return $ joinGap (+>+) pfix (freeGap (tokreplace f_fp toks old new :>: NilFL))
            where f_fp = toFilePath f

replaceCmd _ _ = fail "Usage: darcs replace OLD NEW [FILES]"

floatSubPath :: SubPath -> AnchoredPath
floatSubPath = floatPath . fn2fp . sp2fn

maybeApplyToTree :: Apply p => p C(x y) -> Tree IO -> IO (Maybe (Tree IO))
maybeApplyToTree patch tree = catch (Just `fmap` applyToTree patch tree)
                                    (\_ -> return Nothing)

defaultToks :: String
defaultToks = "A-Za-z_0-9"
filenameToks :: String
filenameToks = "A-Za-z_0-9\\-\\."

-- | Given a set of characters and a string, returns true iff the
-- string contains only characters from the set.  A set beginning with
-- a caret (@^@) is treated as a complementary set.
isTok :: String -> String -> Bool
isTok _ "" = False
isTok toks s = and $ map (regChars toks) s

-- | This function checks for @--token-chars@ on the command-line.  If
-- found, it validates the argument and returns it, without the
-- surrounding square brackets.  Otherwise, it returns either
-- 'defaultToks' or 'filenameToks' as explained in 'replaceHelp'.
--
-- Note: Limitations in the current replace patch file format prevents
-- tokens and token-char specifiers from containing any whitespace.
chooseToks :: [DarcsFlag] -> String -> String -> IO String
chooseToks (Toks t:_) a b
    | length t <= 2 =
        bad_token_spec $ "It must contain more than 2 characters, because " ++
                         "it should be enclosed in square brackets"
    | head t /= '[' || last t /= ']' =
        bad_token_spec "It should be enclosed in square brackets"
    | '^' == head tok && length tok == 1 =
        bad_token_spec "Must be at least one character in the complementary set"
    | any isSpace t =
        bad_token_spec "Space is not allowed in the spec"
    | any isSpace a = bad_token_spec $ spacey_token a
    | any isSpace b = bad_token_spec $ spacey_token b
    | not (isTok tok a) = bad_token_spec $ not_a_token a
    | not (isTok tok b) = bad_token_spec $ not_a_token b
    | otherwise          = return tok
    where tok = init $ tail t :: String
          bad_token_spec msg = fail $ "Bad token spec: '"++ t ++"' ("++ msg ++")"
          spacey_token x = x ++ " must not contain any space"
          not_a_token x = x ++ " is not a token, according to your spec"
chooseToks (_:fs) a b = chooseToks fs a b
chooseToks [] a b = if isTok defaultToks a && isTok defaultToks b
                     then return defaultToks
                     else return filenameToks
\end{code}

