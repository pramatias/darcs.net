module Darcs.Patch.SummaryData ( SummDetail(..), SummOp(..) ) where

import Darcs.Patch.FileName ( FileName )

data SummDetail = SummAddDir FileName
                | SummRmDir  FileName
                | SummFile SummOp FileName Int Int Int
                | SummMv   FileName FileName
                | SummNone
  deriving (Ord, Eq)

data SummOp = SummAdd | SummRm | SummMod deriving (Ord, Eq)

