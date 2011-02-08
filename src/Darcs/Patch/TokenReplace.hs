module Darcs.Patch.TokenReplace 
    ( tryTokInternal, forceTokReplace ) 
    where 

import Darcs.Patch.RegChars ( regChars )

import ByteStringUtils ( substrPS, linesPS, unlinesPS )

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Maybe ( isNothing )

tryTokInternal :: String -> B.ByteString -> B.ByteString
                 -> B.ByteString -> Maybe [B.ByteString]
tryTokInternal _ o n s | isNothing (substrPS o s) &&
                           isNothing (substrPS n s) = Just [s]
tryTokInternal t o n s =
    case BC.break (regChars t) s of
    (before,s') ->
        case BC.break (not . regChars t) s' of
        (tok,after) ->
            case tryTokInternal t o n after of
            Nothing -> Nothing
            Just rest ->
                if tok == o
                then Just $ before : n : rest
                else if tok == n
                     then Nothing
                     else Just $ before : tok : rest

forceTokReplace :: String -> String -> String
                -> B.ByteString -> Maybe B.ByteString
forceTokReplace t os ns c = Just $ unlinesPS $ map forceReplace $ linesPS c
    where o = BC.pack os
          n = BC.pack ns
          tokchar = regChars t
          toks_and_intratoks ps | B.null ps = []
          toks_and_intratoks ps =
              let (before,s') = BC.break tokchar ps
                  (tok, after) = BC.break (not . tokchar) s'
                  in before : tok : toks_and_intratoks after
          forceReplace ps = B.concat $ map o_t_n $ toks_and_intratoks ps
          o_t_n s | s == o = n
                  | otherwise = s

