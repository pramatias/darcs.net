%  Copyright (C) 2002,2003,2005 David Roundy
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
module Darcs.Commands ( CommandControl( CommandData, HiddenCommand, GroupName ),
                       DarcsCommand( DarcsCommand, commandProgramName, commandName,
                                     commandHelp, commandDescription,
                                     commandBasicOptions, commandAdvancedOptions,
                                     commandCommand,
                                     commandPrereq,
                                     commandExtraArgHelp,
                                     commandExtraArgs,
                                     commandArgdefaults,
                                     commandGetArgPossibilities,
                                     SuperCommand,
                                     commandSubCommands ),
                       commandAlias, commandStub,
                       commandOptions, commandAlloptions,
                       disambiguateCommands, CommandArgs(..),
                       getCommandHelp, getCommandMiniHelp,
                       getSubcommands,
                       usage, usageHelper, subusage, chompNewline,
                       extractCommands,
                       superName,
                       nodefaults,
                       putInfo, putVerbose, putWarning, abortRun
                     ) where

import System.Console.GetOpt( OptDescr, usageInfo )
import Control.Monad (when, unless)

import Data.List ( sort, isPrefixOf )
import Darcs.Arguments ( DarcsFlag(Quiet,Verbose, DryRun), DarcsOption, disable, help,
                         anyVerbosity, noCache, posthookCmd, posthookPrompt,
                         prehookCmd, prehookPrompt, optionFromDarcsOption )
import Darcs.RepoPath ( AbsolutePath, rootDirectory )
import Printer ( Doc, putDocLn, hPutDocLn, text, (<+>), errorDoc )
import System.IO ( stderr )
\end{code}

The general format of a darcs command is
\begin{verbatim}
% darcs COMMAND OPTIONS ARGUMENTS ...
\end{verbatim}
Here \verb|COMMAND| is a command such as \verb|add| or \verb|record|, which of
course may have one or more arguments.  Options have the form
\verb!--option! or \verb!-o!, while arguments vary from command to
command.  There are many options which are common to a number of different
commands, which will be summarized here.

If you wish, you may use any unambiguous beginning of a command name as a
shortcut: for \verb!darcs record!, you could type \verb!darcs recor! or
\verb!darcs rec!, but not \verb!darcs re! since that could be confused with
\verb!darcs replace!, \verb!darcs revert! and \verb!darcs remove!.

In some cases, \verb|COMMAND| actually consists of two words, a
super-command and a subcommand.  For example, the ``display the
manifest'' command has the form \verb|darcs query manifest|.

\paragraph{Command overview}

Not all commands modify the ``patches'' of your repository (that
is, the named patches which other users can pull); some commands only
affect the copy of the source tree you're working on (your ``working
directory''), and some affect both. This table summarizes what you should
expect from each one and will hopefully serve as guide when you're having
doubts about which command to use.

\begin{center}
\footnotetext[1]{But it affects the repository and working directory targeted
  by the push}
\footnotetext[2]{As for the other end, see apply}
\begin{tabular}{|c|c|c|}
\hline
affects & patches & working directory\\
\hline
record & yes & no\\
\hline
unrecord & yes & no\\
\hline
rollback & yes & yes\\
\hline
revert & no & yes\\
\hline
unrevert & no & yes\\
\hline
pull & yes & yes\\
\hline
obliterate & yes & yes\\
\hline
apply & yes & yes\\
\hline
push\footnote{But it affects the repository and working directory targeted by
the push} & no & no\\
\hline
send\footnote{As for the other end, see apply} & no & no\\
\hline
put\footnote{Creates a new repository} & no & no\\
\hline
\end{tabular}
\end{center}

\begin{code}
extractCommands, extractHiddenCommands :: [CommandControl] -> [DarcsCommand]
extractCommands cs = concatMap (\x -> case x of { CommandData cmd_d -> [cmd_d]; _ -> []}) cs
extractHiddenCommands cs = concatMap (\x -> case x of { HiddenCommand cmd_d -> [cmd_d]; _ -> []}) cs
\end{code}

\input{Darcs/Arguments.lhs}

\begin{code}
data CommandControl = CommandData DarcsCommand
                    | HiddenCommand DarcsCommand
                    | GroupName String

data DarcsCommand =
    DarcsCommand {commandProgramName, commandName, commandHelp, commandDescription :: String,
                  commandExtraArgs :: Int,
                  commandExtraArgHelp :: [String],
                  commandCommand :: [DarcsFlag] -> [String] -> IO (),
                  commandPrereq :: [DarcsFlag] -> IO (Either String ()),
                  commandGetArgPossibilities :: IO [String],
                  commandArgdefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String],
                  commandBasicOptions :: [DarcsOption],
                  commandAdvancedOptions :: [DarcsOption]}
  | SuperCommand {commandProgramName, commandName, commandHelp, commandDescription :: String,
                  commandPrereq :: [DarcsFlag] -> IO (Either String ()),
                  commandSubCommands :: [CommandControl]}

commandAlloptions :: DarcsCommand -> ([DarcsOption], [DarcsOption])
commandAlloptions DarcsCommand { commandBasicOptions = opts1
                                , commandAdvancedOptions = opts2 }
    = (opts1 ++ [disable, help],
       anyVerbosity ++ opts2 ++
                [noCache
                ,posthookCmd, posthookPrompt
                ,prehookCmd, prehookPrompt])

--  Supercommands cannot be disabled.
commandAlloptions SuperCommand { } = ([help],[])

--  Obtain options suitable as input to
--  System.Console.Getopt, including the --disable option (which is
--  not listed explicitly in the DarcsCommand definitions).
commandOptions :: AbsolutePath -> DarcsCommand -> ([OptDescr DarcsFlag], [OptDescr DarcsFlag])
commandOptions cwd c = (convert basic, convert advanced)
 where (basic, advanced) = commandAlloptions c
       convert = concatMap (optionFromDarcsOption cwd)

nodefaults :: [DarcsFlag] -> AbsolutePath -> [String] -> IO [String]
nodefaults _ _ xs = return xs

getSubcommands :: DarcsCommand -> [CommandControl]
getSubcommands c@(SuperCommand {}) = commandSubCommands c
getSubcommands _ = []

commandAlias :: String -> Maybe DarcsCommand -> DarcsCommand -> DarcsCommand
commandAlias n msuper c =
  c { commandName = n
    , commandDescription = "Alias for `" ++ commandProgramName c ++ " " ++ cmdName ++ "'."
    , commandHelp = "The `" ++ commandProgramName c ++ " " ++ n ++ "' command is an alias for " ++
                     "`" ++ commandProgramName c ++ " " ++ cmdName ++ "'.\n" ++
                     commandHelp c
    }
 where
  cmdName = unwords . map commandName . maybe id (:) msuper $ [ c ]

commandStub :: String -> String -> String -> DarcsCommand -> DarcsCommand
commandStub n h d c =
  c { commandName = n
    , commandHelp = h
    , commandDescription = d
    , commandCommand = \_ _ -> putStr h
    }

usage :: [CommandControl] -> String
usage cs = "Usage: darcs COMMAND ...\n\nCommands:\n" ++
           usageHelper cs ++ "\n" ++
           "Use 'darcs COMMAND --help' for help on a single command.\n" ++
           "Use 'darcs --version' to see the darcs version number.\n" ++
           "Use 'darcs --exact-version' to get the exact version of this darcs instance.\n" ++
           "Use 'darcs help patterns' for help on patch matching.\n" ++
           "Use 'darcs help environment' for help on environment variables.\n" ++
           "\n" ++
           "Check bug reports at http://bugs.darcs.net/\n"

subusage :: DarcsCommand -> String
subusage super =
    (usageInfo
     ("Usage: " ++ commandProgramName super ++ " "++commandName super++" SUBCOMMAND ... " ++
      "\n\n"++ commandDescription super++
      "\n\nSubcommands:\n" ++ usageHelper (getSubcommands super) ++ "\nOptions:")
     (optionFromDarcsOption rootDirectory help))
    ++ "\n" ++ commandHelp super

usageHelper :: [CommandControl] -> String
usageHelper [] = ""
usageHelper (HiddenCommand _:cs) = usageHelper cs
usageHelper ((CommandData c):cs) = "  "++padSpaces (commandName c) 15 ++
                      chompNewline (commandDescription c)++"\n"++usageHelper cs
usageHelper ((GroupName n):cs) = "\n" ++ n ++ "\n" ++ usageHelper cs

chompNewline :: String -> String
chompNewline "" = ""
chompNewline s = if last s == '\n' then init s else s

padSpaces :: String -> Int -> String
padSpaces s n = s ++ replicate (n - length s) ' '

superName :: Maybe DarcsCommand -> String
superName Nothing  = ""
superName (Just x) = commandName x ++ " "

getCommandMiniHelp :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandMiniHelp msuper cmd =
  getCommandHelpCore msuper cmd ++
  "\n\nSee " ++ commandProgramName cmd ++ " help "
  ++ (maybe "" (\c -> commandName c ++ " ") msuper)
  ++ commandName cmd ++ " for details."

getCommandHelp :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandHelp msuper cmd =
    unlines (reverse basicR)
    ++ (if null advanced then ""
        else "\nAdvanced options:\n" ++ unlines (reverse advancedR))
    ++ "\n" ++ commandHelp cmd
    where -- we could just call usageInfo twice, but then the advanced
          -- options might not line up with the basic ones (no short flags)
          (advancedR, basicR) =
             splitAt (length advanced) $ reverse $ lines combinedUsage
          combinedUsage = usageInfo
            (getCommandHelpCore msuper cmd ++ subcommands ++ "\n\nOptions:")
            (basic ++ advanced)
          (basic, advanced) = commandOptions rootDirectory cmd
          subcommands =
            case msuper of
            Nothing -> case getSubcommands cmd of
                       [] -> []
                       s  -> "\n\nSubcommands:\n" ++ (usageHelper s)
            -- we don't want to list subcommands if we're already specifying them
            Just _  -> ""

getCommandHelpCore :: Maybe DarcsCommand -> DarcsCommand -> String
getCommandHelpCore msuper cmd =
    "Usage: " ++ commandProgramName cmd ++ " "++superName msuper++commandName cmd++
    " [OPTION]... " ++ unwords args_help ++
    "\n"++ commandDescription cmd
    where args_help = case cmd of
            (DarcsCommand {}) ->
              commandExtraArgHelp cmd
            _ -> []

data CommandArgs = CommandOnly      DarcsCommand
                 | SuperCommandOnly DarcsCommand
                 | SuperCommandSub  DarcsCommand DarcsCommand

-- Parses a darcs command line with potentially abbreviated commands
disambiguateCommands :: [CommandControl] -> String -> [String]
                      -> Either String (CommandArgs, [String])
disambiguateCommands allcs cmd args =
 do c <- extract cmd allcs
    case (getSubcommands c, args) of
      ([], _)         -> return (CommandOnly c, args)
      (_ ,[])         -> return (SuperCommandOnly c, args)
      (subcs, (a:as)) -> case extract a subcs of
                         Left _   -> return (SuperCommandOnly c, args)
                         Right sc -> return (SuperCommandSub c sc, as)

extract :: String -> [CommandControl] -> Either String DarcsCommand
extract cmd cs =
 case [ c | c <- extractCommands cs, cmd `isPrefixOf` commandName c ] ++
      [ h | h <- extractHiddenCommands cs,    cmd == commandName h ] of
   []  -> Left $ "No such command '" ++ cmd ++ "'\n"
   [c] -> Right c
   cs' -> Left $ "Ambiguous command...\n\n" ++
                    "The command '"++cmd++"' could mean one of:\n" ++
                    unwords (sort $ map commandName cs')

amVerbose :: [DarcsFlag] -> Bool
amVerbose = elem Verbose

amQuiet :: [DarcsFlag] -> Bool
amQuiet = elem Quiet

putVerbose :: [DarcsFlag] -> Doc -> IO ()
putVerbose opts = when (amVerbose opts) . putDocLn

putInfo :: [DarcsFlag] -> Doc -> IO ()
putInfo opts = unless (amQuiet opts) . putDocLn

putWarning :: [DarcsFlag] -> Doc -> IO ()
putWarning opts = unless (amQuiet opts) . hPutDocLn stderr

abortRun :: [DarcsFlag] -> Doc -> IO ()
abortRun opts msg = if DryRun `elem` opts
                    then putInfo opts $ text "NOTE:" <+> msg
                    else errorDoc msg
\end{code}
