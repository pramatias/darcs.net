-- Copyright (C) 2002,2003,2005 David Roundy
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
module Darcs.RunCommand ( runTheCommand ) where

import Control.Monad ( unless, when )
import System.Console.GetOpt( ArgOrder( Permute, RequireOrder ),
                              OptDescr( Option ),
                              getOpt )
import System.Exit ( ExitCode ( ExitSuccess ), exitWith )

import Darcs.Arguments ( DarcsFlag(..),
                         help,
                         fixUrlFlag,
                         optionFromDarcsOption,
                         listOptions, nubOptions )
import Darcs.ArgumentDefaults ( getDefaultFlags )
import Darcs.Commands ( CommandArgs( CommandOnly, SuperCommandOnly, SuperCommandSub ),
                        CommandControl,
                        DarcsCommand,
                        commandName,
                        commandCommand,
                        commandPrereq,
                        commandExtraArgHelp,
                        commandExtraArgs,
                        commandArgdefaults,
                        commandGetArgPossibilities,
                        commandOptions, commandAlloptions,
                        disambiguateCommands,
                        getCommandHelp, getCommandMiniHelp,
                        getSubcommands,
                        extractCommands,
                        superName,
                        subusage, chompNewline )
import Darcs.Commands.GZCRCs ( doCRCWarnings )
import Darcs.Global ( atexit )
import Darcs.External ( viewDoc )
import Darcs.Global ( setDebugMode, setSshControlMasterDisabled,
                      setTimingsMode )
import Darcs.Match ( checkMatchSyntax )
import Progress ( setProgressMode )
import Darcs.RepoPath ( getCurrentDirectory )
import Darcs.Test ( runPosthook, runPrehook )
import Darcs.Utils ( formatPath )
import Data.List ( intersperse )
import Printer ( text )
import URL ( setDebugHTTP, disableHTTPPipelining )

runTheCommand :: [CommandControl] -> String -> [String] -> IO ()
runTheCommand commandControlList cmd args =
  either fail rtc $ disambiguateCommands commandControlList cmd args
 where
  rtc (CommandOnly c, as)       = runCommand Nothing c  as
  rtc (SuperCommandOnly c,  as) = runRawSupercommand c as
  rtc (SuperCommandSub c s, as) = runCommand (Just c) s as

-- This is the actual heavy lifter code, which is responsible for parsing the
-- arguments and then running the command itself.

runCommand :: Maybe DarcsCommand -> DarcsCommand -> [String] -> IO ()

runCommand _ _ args -- Check for "dangerous" typoes...
    | "-all" `elem` args = -- -all indicates --all --look-for-adds!
        fail "Are you sure you didn't mean --all rather than -all?"
runCommand msuper cmd args = do
   cwd <- getCurrentDirectory
   let options = opts1 ++ opts2
       (opts1, opts2) = commandOptions cwd cmd
   case getOpt Permute
             (optionFromDarcsOption cwd listOptions++options) args of
    (opts,extra,[])
      | Help `elem` opts -> viewDoc $ text $ getCommandHelp msuper cmd
      | ListOptions `elem` opts  -> do
           setProgressMode False
-- Warning:  A do-notation statement discarded a result of type Either String ().
           _ <- commandPrereq cmd opts
           file_args <- commandGetArgPossibilities cmd
           putStrLn $ unlines $ getOptionsOptions (opts1++opts2) : file_args
      | otherwise -> considerRunning msuper cmd (addVerboseIfDebug opts) extra
    (_,_,ermsgs) -> do fail $ chompNewline(unlines ermsgs)
    where addVerboseIfDebug opts | DebugVerbose `elem` opts = Debug:Verbose:opts
                                 | otherwise = opts

considerRunning :: Maybe DarcsCommand -> DarcsCommand
                 -> [DarcsFlag] -> [String] -> IO ()
considerRunning msuper cmd opts old_extra = do
 cwd <- getCurrentDirectory
 location <- commandPrereq cmd opts
 case location of
   Left complaint -> fail $ "Unable to " ++
                     formatPath ("darcs " ++ superName msuper ++ commandName cmd) ++
                     " here.\n\n" ++ complaint
   Right () -> do
    specops <- nubopts `fmap` addCommandDefaults cmd opts
    extra <- (commandArgdefaults cmd) specops cwd old_extra
    when (Disable `elem` specops) $
      fail $ "Command "++commandName cmd++" disabled with --disable option!"
    if commandExtraArgs cmd < 0
      then runWithHooks specops extra
      else if length extra > commandExtraArgs cmd
           then fail $ "Bad argument: `"++unwords extra++"'\n"++
                       getCommandMiniHelp msuper cmd
           else if length extra < commandExtraArgs cmd
                then fail $ "Missing argument:  " ++
                            nth_arg (length extra + 1) ++
                            "\n" ++ getCommandMiniHelp msuper cmd
                else runWithHooks specops extra
       where nubopts = nubOptions (uncurry (++) $ commandAlloptions cmd)
             nth_arg n = nth_of n (commandExtraArgHelp cmd)
             nth_of 1 (h:_) = h
             nth_of n (_:hs) = nth_of (n-1) hs
             nth_of _ [] = "UNDOCUMENTED"
             runWithHooks os ex = do
               here <- getCurrentDirectory
               checkMatchSyntax os
               -- set any global variables
               when (Timings `elem` os) setTimingsMode
               when (Debug `elem` os) setDebugMode
               when (DebugHTTP `elem` os) setDebugHTTP
               when (Quiet `elem` os) $ setProgressMode False
               when (NoHTTPPipelining `elem` os) $ disableHTTPPipelining
               unless (SSHControlMaster `elem` os) setSshControlMasterDisabled
               unless (Quiet `elem` os) $ atexit $ doCRCWarnings (Verbose `elem` os)
               -- actually run the command and its hooks
               preHookExitCode <- runPrehook os here
               if preHookExitCode /= ExitSuccess
                  then exitWith preHookExitCode
                  else do let fixFlag = FixFilePath here cwd
                          fixedOs <- mapM (fixUrlFlag [fixFlag]) os
                          (commandCommand cmd) (fixFlag : fixedOs) ex
                          postHookExitCode <- runPosthook os here
                          exitWith postHookExitCode

addCommandDefaults :: DarcsCommand -> [DarcsFlag] -> IO [DarcsFlag]
addCommandDefaults cmd already = do
  let (opts1, opts2) = commandAlloptions cmd
  defaults <- getDefaultFlags (commandName cmd) (opts1 ++ opts2) already
  return $ already ++ defaults

getOptionsOptions :: [OptDescr DarcsFlag] -> String
getOptionsOptions = concat . intersperse "\n" . concatMap goo
 where
  goo (Option _ os _ _) = map ("--"++) os

runRawSupercommand :: DarcsCommand -> [String] -> IO ()
runRawSupercommand super [] =
    fail $ "Command '"++ commandName super ++"' requires subcommand!\n\n"
             ++ subusage super
runRawSupercommand super args = do
  cwd <- getCurrentDirectory
  case getOpt RequireOrder
             (optionFromDarcsOption cwd help++
              optionFromDarcsOption cwd listOptions) args of
    (opts,_,[])
      | Help `elem` opts ->
            viewDoc $ text $ getCommandHelp Nothing super
      | ListOptions `elem` opts -> do
            putStrLn "--help"
            mapM_ (putStrLn . commandName) (extractCommands $ getSubcommands super)
      | otherwise ->
            if Disable `elem` opts
            then fail $ "Command " ++ (commandName super) ++
                      " disabled with --disable option!"
            else fail $ "Invalid subcommand!\n\n" ++ subusage super
    (_,_,ermsgs) -> do fail $ chompNewline(unlines ermsgs)
