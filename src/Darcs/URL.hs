{-# LANGUAGE CPP #-}

{-
Copyright (C) 2004 David Roundy

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.
-}

{-|

  Path resolving:

    * An http URL contains the sequence @\"http(s):\/\/\"@.

    * A local filepath does not contain colons, except
      as second character (windows drives) when this
      filepath is meant to be used as repository name

    * A path that is neither an http URL nor a local file
      is an ssh-path.

  Examples:

  > /usr/repo/foo                 -- local file
  > c:/src/darcs                  -- local file
  > http://darcs.net/             -- URL
  > peter@host:/path              -- ssh
  > droundy@host:                 -- ssh
  > host:/path                    -- ssh

  This means that single-letter hosts in ssh-paths do not work,
  unless a username is provided.

  Perhaps ssh-paths should use @\"ssh:\/\/user\@host\/path\"@-syntax instead?
-}

module Darcs.URL (
    isFile, isHttpUrl, isSshUrl, isRelative, isAbsolute,
    isSshNopath, SshFilePath, sshRepo, sshUhost, sshFile, urlOf, splitSshUrl
  ) where

import Darcs.Global(darcsdir)
import Data.List ( isPrefixOf, isInfixOf )
import Data.Char ( isSpace )
import qualified System.FilePath as FP (isRelative, isAbsolute, isValid)

#include "impossible.h"

isRelative :: String -> Bool
isRelative "" = bug "Empty filename in isRelative"
isRelative f  = FP.isRelative f

isAbsolute :: String -> Bool
isAbsolute "" = bug "isAbsolute called with empty filename"
isAbsolute f = FP.isAbsolute f

isFile :: String -> Bool
isFile f@(_:_:fou) = ':' `notElem` fou && FP.isValid f
isFile f = FP.isValid f

isHttpUrl :: String -> Bool
isHttpUrl u =
    let u' = dropWhile isSpace u in
            ("http://" `isPrefixOf` u') || ("https://" `isPrefixOf` u')


isSshUrl :: String -> Bool
isSshUrl s = isu' (dropWhile isSpace s)
    where
      isu' s'
          | "ssh://" `isPrefixOf` s' = True
          | "://" `isInfixOf` s' = False
          | otherwise = ":" `isInfixOf` s'

isSshNopath :: String -> Bool
isSshNopath s = case reverse s of
                  ':':x@(_:_:_) -> ':' `notElem` x
                  _ -> False

-- | Gives the (user, host, dir) out of an ssh url
splitSshUrl :: String -> SshFilePath
splitSshUrl s | "ssh://" `isPrefixOf` s =
  let s' = drop (length "ssh://") $ dropWhile isSpace s
      (dir, file) = cleanrepodir '/' s'
  in
  SshFP { sshUhost = takeWhile (/= '/') s'
        , sshRepo = dir
        , sshFile = file }
splitSshUrl s =
  let (dir, file) = cleanrepodir ':' s in
  SshFP { sshUhost = dropWhile isSpace $ takeWhile (/= ':') s
        , sshRepo = dir
        , sshFile = file }


cleanrepourl :: String -> (String, String)
cleanrepourl zzz | dd `isPrefixOf` zzz = ([], drop (length dd) zzz)
                 where dd = darcsdir++"/"
cleanrepourl (z:zs) =
  let (repo',file) = cleanrepourl zs in
  (z : repo', file)
cleanrepourl "" = ([],[])

cleanrepodir :: Char -> String -> (String, String)
cleanrepodir sep = cleanrepourl . drop 1 . dropWhile (/= sep)

data SshFilePath = SshFP { sshUhost :: String
                        , sshRepo :: String
                        , sshFile :: String}

urlOf :: SshFilePath -> String
urlOf (SshFP uhost dir file) = uhost ++ ":" ++ dir ++ "/" ++ darcsdir ++ "/" ++ file