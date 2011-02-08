%  Copyright (C) 2002-2004 David Roundy
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
module Darcs.Repository.Motd (getMotd, showMotd) where
import Control.Monad ( unless )
import Darcs.Flags ( DarcsFlag( Quiet, XMLOutput ) )
import Darcs.External ( fetchFilePS, Cachable(..) )
import Darcs.Global ( darcsdir )
import qualified Data.ByteString as B (null, hPut, empty, ByteString)
import Darcs.Utils ( catchall )
import System.IO ( stdout )
\end{code}

\paragraph{motd}\label{motd}
The \verb!_darcs/prefs/motd! file may contain a ``message of the day''
which will be displayed to users who get or pull from the repository without the
\verb!--quiet! option.

\begin{code}
-- | Fetch and return the message of the day for a given repository.
getMotd :: String -> IO B.ByteString
getMotd repo = fetchFilePS (repo++"/"++darcsdir++"/prefs/motd") (MaxAge 600)
                     `catchall` return B.empty

-- | Display the message of the day for a given repository,
--   unless either the 'XMLOutput' or the 'Quiet' flags are passed in
showMotd :: [DarcsFlag] -> String -> IO ()
showMotd opts repo = unless (Quiet `elem` opts || XMLOutput `elem` opts) $ do
  motd <- getMotd repo
  unless (B.null motd)
      $ do B.hPut stdout motd
           putStrLn "**********************"
\end{code}
