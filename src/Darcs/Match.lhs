%  Copyright (C) 2004-2005 David Roundy
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
{-# LANGUAGE CPP, ScopedTypeVariables, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

#include "gadts.h"

-- | /First matcher, Second matcher and Nonrange matcher/
--
-- When we match for patches, we have a PatchSet, of which we want a
-- subset. This subset is formed by the patches in a given interval
-- which match a given criterion. If we represent time going left to
-- right, then we have (up to) three 'Matcher's:
--
-- * the 'firstMatcher' is the left bound of the interval,
--
-- * the 'secondMatcher' is the right bound, and
--
-- * the 'nonrangeMatcher' is the criterion we use to select among
--   patches in the interval.
---
-- Each of these matchers can be present or not according to the
-- options. The patches we want would then be the ones that all
-- present matchers have in common.
--
-- (Implementation note: keep in mind that the PatchSet is written
-- backwards with respect to the timeline, ie., from right to left)
module Darcs.Match ( matchFirstPatchset, matchSecondPatchset,
               matchPatch,
               matchAPatch, matchAPatchread,
               getFirstMatch, getNonrangeMatch,
               getPartialFirstMatch, getPartialSecondMatch,
               getPartialNonrangeMatch,
               firstMatch, secondMatch, haveNonrangeMatch,
               havePatchsetMatch, getOnePatchset,
               checkMatchSyntax, applyInvToMatcher, nonrangeMatcher,
               InclusiveOrExclusive(..), matchExists
             ) where

import Text.Regex ( mkRegex, matchRegex )
import Control.Monad ( when )
import Data.Maybe ( isJust )
import Data.List ( isPrefixOf )

import Darcs.Patch.PatchInfoAnd ( PatchInfoAnd, info, piap,
                         conscientiously, hopefully )
import Darcs.Patch.Info ( justName )
import Darcs.Patch ( RepoPatch, Patchy, Named, invert, invertRL, patch2patchinfo, apply )
import Darcs.Patch.Dummy ( DummyPatch )
import Darcs.Repository ( Repository, readRepo, createPristineDirectoryTree )
import Darcs.Patch.Set ( PatchSet(..), Tagged(..), SealedPatchSet, newset2RL )
#ifdef GADT_WITNESSES
import Darcs.Patch.Set ( Origin )
#endif
import Darcs.Repository.ApplyPatches ( applyPatches )
import Darcs.Patch.Depends ( getPatchesBeyondTag )
import Darcs.Witnesses.Ordered ( RL(..), consRLSealed )

import ByteStringUtils ( mmapFilePS )

import Darcs.Flags ( DarcsFlag( OnePatch, SeveralPatch, Context,
                               AfterPatch, UpToPatch, LastN, PatchIndexRange,
                               OneTag, AfterTag, UpToTag,
                               OnePattern, SeveralPattern,
                               AfterPattern, UpToPattern ) )
import Darcs.Patch.Bundle ( scanContext )
import Darcs.Patch.Match ( Matcher, MatchFun, matchPattern, applyMatcher, makeMatcher, parseMatch )
import Darcs.Patch.MatchData ( PatchMatch )
import Printer ( text, ($$) )

import Darcs.RepoPath ( toFilePath )
import Darcs.IO ( WriteableDirectory(..) )
import Darcs.Patch.FileName ( FileName )
import Darcs.Witnesses.Sealed ( FlippedSeal(..), Sealed2(..),
                      seal, flipSeal, seal2, unsealFlipped, unseal2, unseal )
#include "impossible.h"
\end{code}

\paragraph{Selecting patches}\label{selecting}

Many commands operate on a patch or patches that have already been recorded.
There are a number of options that specify which patches are selected for
these operations: \verb!--patch!, \verb!--match!, \verb!--tag!, and variants
on these, which for \verb!--patch! are \verb!--patches!,
\verb!--from-patch!, and \verb!--to-patch!.  The \verb!--patch! and
\verb!--tag! forms simply take (POSIX extended, aka \verb!egrep!) regular
expressions and match them against tag and patch names.  \verb!--match!,
described below, allows more powerful patterns.

The plural forms of these options select all matching patches.  The singular
forms select the last matching patch.  The range (from and to) forms select
patches after or up to (both inclusive) the last matching patch.

These options use the current order of patches in the repository.  darcs may
reorder patches, so this is not necessarily the order of creation or the
order in which patches were applied.  However, as long as you are just
recording patches in your own repository, they will remain in order.

% NOTE --no-deps is implemented in SelectChanges.lhs, but documented here
% for concistency.
When a patch or a group of patches is selected, all patches they depend on
get silently selected too. For example: \verb!darcs pull --patches bugfix!
means ``pull all the patches with `bugfix' in their name, along with any
patches they require.''  If you really only want patches with `bugfix' in
their name, you should use the \verb!--no-deps! option, which makes darcs
exclude any matched patches from the selection which have dependencies that
are themselves not explicitly matched by the selection.

For \verb!unrecord!, \verb!unpull! and \verb!obliterate!, patches that
depend on the selected patches are silently included, or if
\verb!--no-deps! is used selected patches with dependencies on not selected
patches are excluded from the selection.

\begin{code}
data InclusiveOrExclusive = Inclusive | Exclusive deriving Eq

-- | @haveNonrangeMatch flags@ tells whether there is a flag in
-- @flags@ which corresponds to a match that is "non-range". Thus,
-- @--match@, @--patch@ and @--index@ make @haveNonrangeMatch@
-- true, but not @--from-patch@ or @--to-patch@.
haveNonrangeMatch :: [DarcsFlag] -> Bool
haveNonrangeMatch fs = isJust (hasIndexRange fs) || isJust (nonrangeMatcher fs::Maybe (Matcher DummyPatch))

-- | @havePatchsetMatch flags@ tells whether there is a "patchset
-- match" in the flag list. A patchset match is @--match@ or
-- @--patch@, or @--context@, but not @--from-patch@ nor (!)
-- @--index@.
-- Question: Is it supposed not to be a subset of @haveNonrangeMatch@?
havePatchsetMatch :: [DarcsFlag] -> Bool
havePatchsetMatch fs = isJust (nonrangeMatcher fs::Maybe (Matcher DummyPatch)) || hasC fs
    where hasC [] = False
          hasC (Context _:_) = True
          hasC (_:xs) = hasC xs

getNonrangeMatch :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> IO ()
getNonrangeMatch r fs = withRecordedMatch r (getNonrangeMatchS fs)

getPartialNonrangeMatch :: RepoPatch p => Repository p C(r u t)
                           -> [DarcsFlag] -> [FileName] -> IO ()
getPartialNonrangeMatch r fs _ =
    withRecordedMatch r (getNonrangeMatchS fs)

getNonrangeMatchS :: (RepoPatch p) =>
                        [DarcsFlag] -> PatchSet p C(Origin x) -> IO ()
getNonrangeMatchS fs repo =
    case nonrangeMatcher fs of
        Just m -> if nonrangeMatcherIsTag fs
                        then getTagS m repo
                        else getMatcherS Exclusive m repo
        Nothing -> fail "Pattern not specified in getNonrangeMatch."

-- | @firstMatch fs@ tells whether @fs@ implies a "first match", that
-- is if we match against patches from a point in the past on, rather
-- than against all patches since the creation of the repository.
firstMatch :: [DarcsFlag] -> Bool
firstMatch fs = isJust (hasLastn fs)
                 || isJust (firstMatcher fs::Maybe (Matcher DummyPatch))
                 || isJust (hasIndexRange fs)

getFirstMatch :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] -> IO ()
getFirstMatch r fs = withRecordedMatch r (getFirstMatchS fs)

getPartialFirstMatch :: RepoPatch p => Repository p C(r u t)
                        -> [DarcsFlag] -> [FileName] -> IO ()
getPartialFirstMatch r fs _ =
    withRecordedMatch r (getFirstMatchS fs)

getFirstMatchS :: (RepoPatch p) =>
                     [DarcsFlag] -> PatchSet p C(Origin x) -> IO ()
getFirstMatchS fs repo =
    case hasLastn fs of
    Just n -> unpullLastN repo n
    Nothing ->
     case hasIndexRange fs of
     Just (_,b) -> unpullLastN repo b -- b is chronologically earlier than a
     Nothing    ->
      case firstMatcher fs of
               Nothing -> fail "Pattern not specified in getFirstMatch."
               Just m -> if firstMatcherIsTag fs
                         then getTagS m repo
                         else getMatcherS Inclusive m repo

-- | @secondMatch fs@ tells whether @fs@ implies a "second match", that
-- is if we match against patches up to a point in the past on, rather
-- than against all patches until now.
secondMatch :: [DarcsFlag] -> Bool
secondMatch fs = isJust (secondMatcher fs::Maybe (Matcher DummyPatch)) || isJust (hasIndexRange fs)

getPartialSecondMatch :: RepoPatch p => Repository p C(r u t)
                        -> [DarcsFlag] -> [FileName] -> IO ()
getPartialSecondMatch r fs _ =
    withRecordedMatch r $ \repo ->
    case secondMatcher fs of
    Nothing -> case hasIndexRange fs of
                Just (a,_) -> unpullLastN repo (a-1)
                Nothing    -> fail "Two patterns not specified in get_second_match."
    Just m -> if secondMatcherIsTag fs
              then getTagS m repo
              else getMatcherS Exclusive m repo

unpullLastN :: Patchy p => PatchSet p C(x y) -> Int -> IO ()
unpullLastN repo n = applyInvRL `unsealFlipped` (safetake n $ newset2RL repo)

checkMatchSyntax :: [DarcsFlag] -> IO ()
checkMatchSyntax opts = do
 case getMatchPattern opts of
  Nothing -> return ()
  Just p  -> either fail (const $ return ()) $ (parseMatch p::Either String (MatchFun DummyPatch))

getMatchPattern :: [DarcsFlag] -> Maybe PatchMatch
getMatchPattern [] = Nothing
getMatchPattern (OnePattern m:_) = Just m
getMatchPattern (SeveralPattern m:_) = Just m
getMatchPattern (_:fs) = getMatchPattern fs

tagmatch :: String -> Matcher p
tagmatch r = makeMatcher ("tag-name "++r) tm
    where tm (Sealed2 p) =
              let n = justName (info p) in
              "TAG " `isPrefixOf` n && isJust (matchRegex (mkRegex r) $ drop 4 n)

mymatch :: String -> Matcher p
mymatch r = makeMatcher ("patch-name "++r) mm
    where mm (Sealed2 p) = isJust . matchRegex (mkRegex r) . justName . info $ p


-- | strictJust is a strict version of the Just constructor, used to ensure
-- that if we claim we've got a pattern match, that the pattern will
-- actually match (rathern than fail to compile properly).
strictJust :: a -> Maybe a
strictJust x = Just $! x

-- | @nonrangeMatcher@ is the criterion that is used to match against
-- patches in the interval. It is 'Just m' when the @--patch@, @--match@,
-- @--tag@ options are passed (or their plural variants).
nonrangeMatcher :: Patchy p => [DarcsFlag] -> Maybe (Matcher p)
nonrangeMatcher [] = Nothing
nonrangeMatcher (OnePattern m:_) = strictJust $ matchPattern m
nonrangeMatcher (OneTag t:_) = strictJust $ tagmatch t
nonrangeMatcher (OnePatch p:_) = strictJust $ mymatch p
nonrangeMatcher (SeveralPattern m:_) = strictJust $ matchPattern m
nonrangeMatcher (SeveralPatch p:_) = strictJust $ mymatch p
nonrangeMatcher (_:fs) = nonrangeMatcher fs

-- | @nonrangeMatcherIsTag@ returns true if the matching option was
-- '--tag'
nonrangeMatcherIsTag :: [DarcsFlag] -> Bool
nonrangeMatcherIsTag [] = False
nonrangeMatcherIsTag (OneTag _:_) = True
nonrangeMatcherIsTag (_:fs) = nonrangeMatcherIsTag fs

-- | @firstMatcher@ returns the left bound of the matched interval.
-- This left bound is also specified when we use the singular versions
-- of @--patch@, @--match@ and @--tag@. Otherwise, @firstMatcher@
-- returns @Nothing@.
firstMatcher :: Patchy p => [DarcsFlag] -> Maybe (Matcher p)
firstMatcher [] = Nothing
firstMatcher (OnePattern m:_) = strictJust $ matchPattern m
firstMatcher (AfterPattern m:_) = strictJust $ matchPattern m
firstMatcher (AfterTag t:_) = strictJust $ tagmatch t
firstMatcher (OnePatch p:_) = strictJust $ mymatch p
firstMatcher (AfterPatch p:_) = strictJust $ mymatch p
firstMatcher (_:fs) = firstMatcher fs

firstMatcherIsTag :: [DarcsFlag] -> Bool
firstMatcherIsTag [] = False
firstMatcherIsTag (AfterTag _:_) = True
firstMatcherIsTag (_:fs) = firstMatcherIsTag fs

secondMatcher :: Patchy p => [DarcsFlag] -> Maybe (Matcher p)
secondMatcher [] = Nothing
secondMatcher (OnePattern m:_) = strictJust $ matchPattern m
secondMatcher (UpToPattern m:_) = strictJust $ matchPattern m
secondMatcher (OnePatch p:_) = strictJust $ mymatch p
secondMatcher (UpToPatch p:_) = strictJust $ mymatch p
secondMatcher (UpToTag t:_) = strictJust $ tagmatch t
secondMatcher (_:fs) = secondMatcher fs

secondMatcherIsTag :: [DarcsFlag] -> Bool
secondMatcherIsTag [] = False
secondMatcherIsTag (UpToTag _:_) = True
secondMatcherIsTag (_:fs) = secondMatcherIsTag fs

-- | @matchAPatchread fs p@ tells whether @p@ matches the matchers in
-- the flags listed in @fs@.
matchAPatchread :: Patchy p => [DarcsFlag] -> PatchInfoAnd p C(x y) -> Bool
matchAPatchread fs = case nonrangeMatcher fs of
                       Nothing -> const True
                       Just m -> applyMatcher m

-- | @matchAPatch fs p@ tells whether @p@ matches the matchers in
-- the flags @fs@
matchAPatch :: Patchy p => [DarcsFlag] -> Named p C(x y) -> Bool
matchAPatch fs p =
    case nonrangeMatcher fs of
    Nothing -> True
    Just m -> applyMatcher m (patch2patchinfo p `piap` p)

matchPatch :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x) -> Sealed2 (Named p)
matchPatch fs ps =
    case hasIndexRange fs of
    Just (a,a') | a == a' -> case (unseal myhead) $ dropn (a-1) ps of
                             Just (Sealed2 p) -> seal2 $ hopefully p
                             Nothing -> error "Patch out of range!"
                | otherwise -> bug ("Invalid index range match given to matchPatch: "++
                                    show (PatchIndexRange a a'))
                where myhead :: PatchSet p C(start x) -> Maybe (Sealed2 (PatchInfoAnd p))
                      myhead (PatchSet NilRL (Tagged t _ _ :<: _)) = Just $ seal2 t
                      myhead (PatchSet (x:<:_) _) = Just $ seal2 x
                      myhead _ = Nothing
    Nothing -> case nonrangeMatcher fs of
                    Nothing -> bug "Couldn't matchPatch"
                    Just m -> findAPatch m ps

getOnePatchset :: RepoPatch p => Repository p C(r u t) -> [DarcsFlag] ->
                 IO (SealedPatchSet p C(Origin))
getOnePatchset repository fs =
    case nonrangeMatcher fs of
        Just m -> do ps <- readRepo repository
                     if nonrangeMatcherIsTag fs
                        then return $ getMatchingTag m ps
                        else return $ matchAPatchset m ps
        Nothing -> (seal . scanContext) `fmap` mmapFilePS (toFilePath $ context_f fs)
    where context_f [] = bug "Couldn't match_nonrange_patchset"
          context_f (Context f:_) = f
          context_f (_:xs) = context_f xs

-- | @hasLastn fs@ return the @--last@ argument in @fs@, if any.
hasLastn :: [DarcsFlag] -> Maybe Int
hasLastn [] = Nothing
hasLastn (LastN (-1):_) = error "--last requires a positive integer argument."
hasLastn (LastN n:_) = Just n
hasLastn (_:fs) = hasLastn fs

hasIndexRange :: [DarcsFlag] -> Maybe (Int,Int)
hasIndexRange [] = Nothing
hasIndexRange (PatchIndexRange x y:_) = Just (x,y)
hasIndexRange (_:fs) = hasIndexRange fs

-- | @matchFirstPatchset fs ps@ returns the part of @ps@ before its
-- first matcher, ie the one that comes first dependencywise. Hence,
-- patches in @matchFirstPatchset fs ps@ are the context for the ones
-- we don't want.
matchFirstPatchset :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x)
                   -> SealedPatchSet p C(start)
matchFirstPatchset fs patchset =
    case hasLastn fs of
    Just n -> dropn n patchset
    Nothing ->
        case hasIndexRange fs of
        Just (_,b) -> dropn b patchset
        Nothing ->
               case firstMatcher fs of
               Nothing -> bug "Couldn't matchFirstPatchset"
               Just m -> unseal (dropn 1) $ if firstMatcherIsTag fs
                                            then getMatchingTag m patchset
                                            else matchAPatchset m patchset

-- | @dropn n ps@ drops the @n@ last patches from @ps@.
dropn :: Int -> PatchSet p C(start x) -> SealedPatchSet p C(start)
dropn n ps | n <= 0 = seal ps
dropn n (PatchSet NilRL (Tagged t _ ps :<: ts)) = dropn n $ PatchSet (t:<:ps) ts
dropn _ (PatchSet NilRL NilRL) = seal $ PatchSet NilRL NilRL
dropn n (PatchSet (_:<:ps) ts) = dropn (n-1) $ PatchSet ps ts

-- | @matchSecondPatchset fs ps@ returns the part of @ps@ before its
-- second matcher, ie the one that comes last dependencywise.
matchSecondPatchset :: RepoPatch p => [DarcsFlag] -> PatchSet p C(start x)
                    -> SealedPatchSet p C(start)
matchSecondPatchset fs ps =
  case hasIndexRange fs of
  Just (a,_) -> dropn (a-1) ps
  Nothing ->
    case secondMatcher fs of
    Nothing -> bug "Couldn't matchSecondPatchset"
    Just m -> if secondMatcherIsTag fs
              then getMatchingTag m ps
              else matchAPatchset m ps

-- | @findAPatch m ps@ returns the last patch in @ps@ matching @m@, and
-- calls 'error' if there is none.
findAPatch :: RepoPatch p => Matcher p -> PatchSet p C(start x) -> Sealed2 (Named p)
findAPatch m (PatchSet NilRL NilRL) = error $ "Couldn't find patch matching " ++ show m
findAPatch m (PatchSet NilRL (Tagged t _ ps :<: ts)) = findAPatch m (PatchSet (t:<:ps) ts)
findAPatch m (PatchSet (p:<:ps) ts) | applyMatcher m p = seal2 $ hopefully p
                                    | otherwise = findAPatch m (PatchSet ps ts)

-- | @matchAPatchset m ps@ returns a (the largest?) subset of @ps@
-- ending in patch which matches @m@. Calls 'error' if there is none.
matchAPatchset :: RepoPatch p => Matcher p -> PatchSet p C(start x)
               -> SealedPatchSet p C(start)
matchAPatchset m (PatchSet NilRL NilRL) = error $ "Couldn't find patch matching " ++ show m
matchAPatchset m (PatchSet NilRL (Tagged t _ ps :<: ts)) = matchAPatchset m (PatchSet (t:<:ps) ts)
matchAPatchset m (PatchSet (p:<:ps) ts) | applyMatcher m p = seal (PatchSet (p:<:ps) ts)
                                        | otherwise = matchAPatchset m (PatchSet ps ts)

-- | @getMatchingTag m ps@, where @m@ is a 'Matcher' which matches tags
-- returns a 'SealedPatchSet' containing all patches in the last tag which
-- matches @m@. Last tag means the most recent tag in repository order,
-- i.e. the last one you'd see if you ran darcs changes -t @m@. Calls
-- 'error' if there is no matching tag.
getMatchingTag :: RepoPatch p => Matcher p -> PatchSet p C(start x) -> SealedPatchSet p C(start)
getMatchingTag m (PatchSet NilRL NilRL) = error $ "Couldn't find a tag matching " ++ show m
getMatchingTag m (PatchSet NilRL (Tagged t _ ps :<: ts)) = getMatchingTag m (PatchSet (t:<:ps) ts)
getMatchingTag m (PatchSet (p:<:ps) ts)
    | applyMatcher m p = seal $ PatchSet (p:<:ps) ts
    | otherwise = getMatchingTag m (PatchSet ps ts)

-- | @matchExists m ps@ tells whether there is a patch matching
-- @m@ in @ps@
matchExists :: Matcher p -> PatchSet p C(start x) -> Bool
matchExists _ (PatchSet NilRL NilRL) = False
matchExists m (PatchSet NilRL (Tagged t _ ps :<: ts)) = matchExists m (PatchSet (t:<:ps) ts)
matchExists m (PatchSet (p:<:ps) ts) | applyMatcher m $ p = True
                                     | otherwise = matchExists m (PatchSet ps ts)

applyInvToMatcher :: (RepoPatch p, WriteableDirectory m) => InclusiveOrExclusive -> Matcher p -> PatchSet p C(Origin x) -> m ()
applyInvToMatcher _ _ (PatchSet NilRL NilRL) = impossible
applyInvToMatcher ioe m (PatchSet NilRL (Tagged t _ ps :<: ts)) = applyInvToMatcher ioe m
                                                                  (PatchSet (t:<:ps) ts)
applyInvToMatcher ioe m (PatchSet (p:<:ps) xs)
    | applyMatcher m p = when (ioe == Inclusive) (applyInvp p)
    | otherwise = applyInvp p >> applyInvToMatcher ioe m (PatchSet ps xs)

getMatcherS :: (WriteableDirectory m, RepoPatch p) =>
                 InclusiveOrExclusive -> Matcher p -> PatchSet p C(Origin x) -> m ()
getMatcherS ioe m repo =
    if matchExists m repo
    then applyInvToMatcher ioe m repo
    else fail $ "Couldn't match pattern "++ show m

getTagS :: (RepoPatch p) =>
             Matcher p -> PatchSet p C(Origin x) -> IO ()
getTagS match repo = do
    let pinfo = patch2patchinfo `unseal2` (findAPatch match repo)
    case getPatchesBeyondTag pinfo repo of
        FlippedSeal extras -> applyInvRL extras

-- | @applyInvp@ tries to get the patch that's in a 'PatchInfoAnd
-- patch', and to apply its inverse. If we fail to fetch the patch
-- (presumably in a partial repositiory), then we share our sorrow
-- with the user.
applyInvp :: (Patchy p, WriteableDirectory m) => PatchInfoAnd p C(x y) -> m ()
applyInvp hp = apply (invert $ fromHopefully hp)
    where fromHopefully = conscientiously $ \e ->
                     text "Sorry, partial repository problem.  Patch not available:"
                     $$ e
                     $$ text ""
                     $$ text "If you think what you're trying to do is ok then"
                     $$ text "report this as a bug on the darcs-user list."

-- | a version of 'take' for 'RL' lists that cater for contexts.
safetake :: Int -> RL a C(x y) -> FlippedSeal (RL a) C(y)
safetake 0 _ = flipSeal NilRL
safetake _ NilRL = error "There aren't that many patches..."
safetake i (a:<:as) = a `consRLSealed` safetake (i-1) as

withRecordedMatch :: RepoPatch p => Repository p C(r u t)
                  -> (PatchSet p C(Origin r) -> IO ()) -> IO ()
withRecordedMatch r job = do createPristineDirectoryTree r "."
                             readRepo r >>= job

applyInvRL :: (Patchy p) => RL (PatchInfoAnd p) C(x r) -> IO ()
applyInvRL = applyPatches . invertRL -- this gives nicer feedback
\end{code}
