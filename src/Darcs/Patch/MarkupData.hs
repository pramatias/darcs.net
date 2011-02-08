module Darcs.Patch.MarkupData
    ( LineMark(..), MarkedUpFile, emptyMarkedupFile
    )
    where

import Data.ByteString as B ( ByteString, empty )

data LineMark pi
    = AddedLine pi | RemovedLine pi
    | AddedRemovedLine pi pi | None
   deriving (Show)
type MarkedUpFile pi = [(B.ByteString, LineMark pi)]
emptyMarkedupFile :: MarkedUpFile pi
emptyMarkedupFile = [(B.empty, None)]
