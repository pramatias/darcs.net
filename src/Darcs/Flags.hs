-- Copyright (C) 2002-2004 David Roundy
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

module Darcs.Flags ( DarcsFlag( .. ),
                     Compression( .. ), UseIndex(..), ScanKnown(..), RemoteDarcs(..),
                     compression, remoteDarcs, diffingOpts,
                     wantExternalMerge, isInteractive,
                     maxCount, willIgnoreTimes, willRemoveLogFile, isUnified,
                     doHappyForwarding, includeBoring,
                     doAllowCaseOnly, doAllowWindowsReserved, doReverse,
                     usePacks,
                     showChangesOnlyToFiles, rollbackInWorkingDir,
                     defaultFlag
                   ) where
import Data.Maybe( fromMaybe )
import Darcs.Patch.MatchData ( PatchMatch )
import Darcs.RepoPath ( AbsolutePath, AbsolutePathOrStd )

-- | The 'DarcsFlag' type is a list of all flags that can ever be
-- passed to darcs, or to one of its commands.
data DarcsFlag = Help | ListOptions | NoTest | Test
               | OnlyChangesToFiles | ChangesToAllFiles
               | LeaveTestDir | NoLeaveTestDir
               | Timings | Debug | DebugVerbose | DebugHTTP
               | Verbose | NormalVerbosity | Quiet
               | Target String | Cc String
               | Output AbsolutePathOrStd | OutputAutoName AbsolutePath
               | Subject String | InReplyTo String
               | SendmailCmd String | Author String | PatchName String
               | OnePatch String | SeveralPatch String
               | AfterPatch String | UpToPatch String
               | TagName String | LastN Int | MaxCount Int | PatchIndexRange Int Int
               | NumberPatches
               | OneTag String | AfterTag String | UpToTag String
               | GenContext | Context AbsolutePath | Count
               | LogFile AbsolutePath | RmLogFile | DontRmLogFile
               | DistName String | All
               | Recursive | NoRecursive | Reorder
               | RestrictPaths | DontRestrictPaths
               | AskDeps | NoAskDeps | IgnoreTimes | DontIgnoreTimes
               | LookForAdds | NoLookForAdds
               | AnyOrder | CreatorHash String
               | Intersection | Union | Complement
               | Sign | SignAs String | NoSign | SignSSL String
               | HappyForwarding | NoHappyForwarding
               | Verify AbsolutePath | VerifySSL AbsolutePath
               | SSHControlMaster | NoSSHControlMaster
               | RemoteDarcsOpt String
               | EditDescription | NoEditDescription
               | Toks String
               | EditLongComment | NoEditLongComment | PromptLongComment
               | KeepDate | NoKeepDate
               | AllowConflicts | MarkConflicts | NoAllowConflicts
               | SkipConflicts
               | Boring | SkipBoring
               | AllowCaseOnly | DontAllowCaseOnly
               | AllowWindowsReserved | DontAllowWindowsReserved
               | DontGrabDeps | DontPromptForDependencies | PromptForDependencies
               | Compress | NoCompress | UnCompress
               | WorkRepoDir String | WorkRepoUrl String | RemoteRepo String
               | NewRepo String
               | Reply String | ApplyAs String
               | MachineReadable | HumanReadable
               | Pipe | Interactive
               | DiffCmd String
               | ExternalMerge String | Summary | NoSummary
               | Unified | NonUnified | Reverse | Forward
               | Complete | Lazy
               | FixFilePath AbsolutePath AbsolutePath | DiffFlags String
               | XMLOutput
               | ForceReplace
               | OnePattern PatchMatch | SeveralPattern PatchMatch
               | AfterPattern PatchMatch | UpToPattern PatchMatch
               | NonApply | NonVerify | NonForce
               | DryRun | SetDefault | NoSetDefault
               | Disable | SetScriptsExecutable | DontSetScriptsExecutable | Bisect
               | UseHashedInventory | UseOldFashionedInventory
               | UseFormat2
               | PristinePlain | NoUpdateWorking
               | Sibling AbsolutePath | Relink
               | OptimizePristine | OptimizeHTTP
               | UpgradeFormat
               | Files | NoFiles | Directories | NoDirectories
               | Pending | NoPending
               | PosthookCmd String | NoPosthook | AskPosthook | RunPosthook
               | PrehookCmd String  | NoPrehook  | AskPrehook  | RunPrehook
               | UMask String
               | StoreInMemory | ApplyOnDisk
               | NoHTTPPipelining
               | Packs | NoPacks
               | NoCache
               | AllowUnrelatedRepos
               | Check | Repair | JustThisRepo
               | NullFlag
               | RecordRollback | NoRecordRollback
                 deriving ( Eq, Show )

-- ADTs for selecting specific behaviour... FIXME These should be eventually
-- moved out from this module, closer to where they are actually used

data Compression = NoCompression | GzipCompression
compression :: [DarcsFlag] -> Compression
compression f | NoCompress `elem` f = NoCompression
              | otherwise = GzipCompression

remoteDarcs :: [DarcsFlag] -> RemoteDarcs
remoteDarcs f | (x:_) <- [ c | RemoteDarcsOpt c <- f ] = RemoteDarcs x
              | otherwise = DefaultRemoteDarcs

data UseIndex = UseIndex | IgnoreIndex
data ScanKnown = ScanKnown | ScanAll
diffingOpts :: [DarcsFlag] -> (UseIndex, ScanKnown)
diffingOpts opts = (if willIgnoreTimes opts then IgnoreIndex else UseIndex,
                    if LookForAdds `elem` opts then ScanAll else ScanKnown)

data RemoteDarcs = RemoteDarcs String | DefaultRemoteDarcs

----------------------

wantExternalMerge :: [DarcsFlag] -> Maybe String
wantExternalMerge [] = Nothing
wantExternalMerge (ExternalMerge c:_) = Just c
wantExternalMerge (_:fs) = wantExternalMerge fs

isInteractive :: [DarcsFlag] -> Bool
isInteractive = isInteractive_ True
    where
      isInteractive_ def [] = def
      isInteractive_ _ (Interactive:_) = True
      isInteractive_ _ (All:_) = False
      isInteractive_ _ (DryRun:fs) = isInteractive_ False fs
      isInteractive_ def (_:fs) = isInteractive_ def fs

maxCount :: [DarcsFlag] -> Maybe Int
maxCount (MaxCount n : _) = Just n
maxCount (_:xs) = maxCount xs
maxCount [] = Nothing

-- | @lastWord [(flag, value)] default opts@ scans @opts@ for a flag
-- in the list and returns the value of the first match, or @default@
-- if none is found.
--
-- We call this the \"last\" word because we assume that flags are
-- *prepended* in the order they arrive, so what is first internally
-- is last from the user's point of view.
lastWord :: [(DarcsFlag,a)] -> a -> [DarcsFlag] -> a
lastWord known_flags = foldr . flip $ \ def -> fromMaybe def . flip lookup known_flags

getBoolFlag :: DarcsFlag -> DarcsFlag -> [DarcsFlag] -> Bool
getBoolFlag t f = lastWord [(t, True), (f, False)] False

willIgnoreTimes :: [DarcsFlag] -> Bool
willIgnoreTimes = getBoolFlag IgnoreTimes DontIgnoreTimes

willRemoveLogFile :: [DarcsFlag] -> Bool
willRemoveLogFile = getBoolFlag RmLogFile DontRmLogFile

isUnified :: [DarcsFlag] -> Bool
isUnified = getBoolFlag Unified NonUnified

doHappyForwarding :: [DarcsFlag] -> Bool
doHappyForwarding = getBoolFlag HappyForwarding NoHappyForwarding

includeBoring :: [DarcsFlag] -> Bool
includeBoring = getBoolFlag Boring SkipBoring

doAllowCaseOnly :: [DarcsFlag] -> Bool
doAllowCaseOnly = getBoolFlag AllowCaseOnly DontAllowCaseOnly


doAllowWindowsReserved :: [DarcsFlag] -> Bool
doAllowWindowsReserved = getBoolFlag AllowWindowsReserved DontAllowWindowsReserved

doReverse :: [DarcsFlag] -> Bool
doReverse = getBoolFlag Reverse Forward

usePacks :: [DarcsFlag] -> Bool
usePacks = getBoolFlag Packs NoPacks

showChangesOnlyToFiles :: [DarcsFlag] -> Bool
showChangesOnlyToFiles = getBoolFlag OnlyChangesToFiles ChangesToAllFiles

-- | Set flags to a default value, but only one has not already been provided
defaultFlag :: [DarcsFlag] -- ^ distractors
            -> DarcsFlag   -- ^ default
            -> [DarcsFlag] -- ^ flags
            -> [DarcsFlag] -- ^ updated flags
defaultFlag alts def flags =
 if any (`elem` flags) alts then flags else def : flags

rollbackInWorkingDir :: [DarcsFlag] -> Bool
rollbackInWorkingDir = getBoolFlag NoRecordRollback RecordRollback
