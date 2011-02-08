{-# LANGUAGE BangPatterns #-}
-- | This module defines our parsing monad.  In the past there have been lazy
-- and strict parsers in this module.  Currently we have only the strict
-- variant and it is used for parsing patch files.
module Darcs.Patch.ReadMonads (ParserM, Darcs.Patch.ReadMonads.take,
                        parse, parseStrictly, char, int,
                        option, choice, skipSpace, skipWhile, string,
                        lexChar, lexString, lexEof, takeTillChar,
                        myLex', anyChar, endOfInput, takeTill,
                        checkConsumes,
                        linesStartingWith, linesStartingWithEndingWith) where

import ByteStringUtils ( dropSpace, breakSpace, breakFirstPS,
                         readIntPS, breakLastPS )
import qualified Data.ByteString as B (null, drop, length, tail, empty,
                                       ByteString)
import qualified Data.ByteString.Char8 as BC ( uncons, dropWhile, break
                                             , splitAt, length, head )
import Control.Applicative ( Alternative(..), Applicative(..), (<$>) )
import Control.Monad ( MonadPlus(..) )

-- | 'lexChar' checks if the next space delimited token from
-- the input stream matches a specific 'Char'.
-- Uses 'Maybe' inside 'ParserM' to handle failed matches, so
-- that it always returns () on success.
lexChar :: ParserM m => Char -> m ()
lexChar c = do
  skipSpace
  char c
  return ()

-- | 'lexString' fetches the next whitespace delimited token from
-- from the input and checks if it matches the 'ByteString' input.
-- Uses 'Maybe' inside 'ParserM' to handle failed matches, so
-- that it always returns () on success.
lexString :: ParserM m => B.ByteString -> m ()
lexString str = work
           $ \s -> case myLex s of
                       Just (xs :*: ys) | xs == str -> Just (() :*: ys)
                       _ -> Nothing

-- | Only succeeds if the characters in the input exactly match @str@.
string :: ParserM m => B.ByteString -> m ()
string str = work
        $ \s -> case BC.splitAt (BC.length str) s of
                  (h, t) | h == str -> Just (() :*: t)
                  _ -> Nothing

-- | 'lexEof' looks for optional spaces followed by the end of input.
-- Uses 'Maybe' inside 'ParserM' to handle failed matches, so
-- that it always returns () on success.
lexEof :: ParserM m => m ()
lexEof = work
        $ \s -> if B.null (dropSpace s)
                then Just (() :*: B.empty)
                else Nothing

-- | 'myLex' drops leading spaces and then breaks the string at the
-- next space.  Returns 'Nothing' when the string is empty after
-- dropping leading spaces, otherwise it returns the first sequence
-- of non-spaces and the remainder of the input.
myLex :: B.ByteString -> Maybe (ParserState B.ByteString)
myLex s = let s' = dropSpace s
           in if B.null s'
              then Nothing
              else Just $ stuple $ breakSpace s'

-- | Like 'myLex' except that it is in ParserM
myLex' :: ParserM m => m B.ByteString
myLex' = work myLex

-- | Accepts the next character and returns it.  Only fails at end of
-- input.
anyChar :: ParserM m => m Char
anyChar = work $ \s -> stuple <$> BC.uncons s

-- | Only succeeds at end of input, consumes no characters.
endOfInput :: ParserM m => m ()
endOfInput = work $ \s -> if B.null s
                            then Just (() :*: s)
                            else Nothing

-- | Accepts only the specified character.  Consumes a character, if
-- available.
char :: ParserM m => Char -> m ()
char c = work $ \s ->
  case BC.uncons s of
  Just (c', s') | c == c' -> Just (() :*: s')
  _ -> Nothing

-- | Parse an integer and return it.  Skips leading whitespaces and
-- | uses the efficient ByteString readInt.
int :: ParserM m => m Int
int = work $ \s -> stuple <$> readIntPS s

-- | Discards spaces until a non-space character is encountered.
-- Always succeeds.
skipSpace :: ParserM m => m ()
skipSpace = alterInput dropSpace

-- | Discards any characters as long as @p@ returns True.  Always
-- | succeeds.
skipWhile :: ParserM m => (Char -> Bool) -> m ()
skipWhile p = alterInput (BC.dropWhile p)

-- | Takes characters while @p@ returns True.  Always succeeds.
takeTill :: ParserM m => (Char -> Bool) -> m B.ByteString
takeTill p = work $ \s -> Just $ stuple (BC.break p s)

-- | Equivalent to @takeTill (==c)@, except that it is optimized for
-- | the equality case.
takeTillChar :: ParserM m => Char -> m B.ByteString
takeTillChar c = work $ \s -> Just $ stuple (BC.break (==c) s)

-- | Takes exactly @n@ bytes, or fails.
take :: ParserM m => Int -> m B.ByteString
take n = work $ \s -> if B.length s >= n
                        then Just $ stuple $ BC.splitAt n s
                        else Nothing

-- | This is a highly optimized way to read lines that start with a
-- particular character.  To implement this efficiently we need access
-- to the parser's internal state.  If this is implemented in terms of
-- the other primitives for the parser it requires us to consume one
-- character at a time.  That leads to @(>>=)@ wasting significant
-- time.
linesStartingWith :: ParserM m => Char -> m [B.ByteString]
linesStartingWith c = work $ linesStartingWith' c

-- | Helper function for 'linesStartingWith'.
linesStartingWith' :: Char -> B.ByteString -> Maybe (ParserState [B.ByteString])
linesStartingWith' c thes =
    Just (lsw [] thes)
    where lsw acc s | B.null s || BC.head s /= c = (reverse acc :*: s)
          lsw acc s = let s' = B.tail s
                  in case breakFirstPS '\n' s' of
                     Just (l, r) -> lsw (l:acc) r
                     Nothing -> (reverse (s':acc) :*: B.empty)

-- | This is a highly optimized way to read lines that start with a
-- particular character, and stops when it reaches a particular |
-- character.  See 'linesStartingWith' for details on why this |
-- defined here as a primitive.
linesStartingWithEndingWith :: ParserM m => Char -> Char -> m [B.ByteString]
linesStartingWithEndingWith st en = work $ linesStartingWithEndingWith' st en

-- | Helper function for 'linesStartingWithEndingWith'.
linesStartingWithEndingWith' :: Char -> Char -> B.ByteString
                             -> Maybe (ParserState [B.ByteString])
linesStartingWithEndingWith' st en s = lswew s
    where
  lswew x | B.null x = Nothing
  lswew x =
    if BC.head x == en
    then Just ([] :*: B.tail x)
    else if BC.head x /= st
         then Nothing
         else case BC.break ((==) '\n') $ B.tail x of
              (l,r) -> case lswew $ B.tail r of
                       Just (ls :*: r') -> Just ((l:ls) :*: r')
                       Nothing ->
                           case breakLastPS en l of
                           Just (l2,_) ->
                               Just ([l2] :*: B.drop (B.length l2+2) x)
                           Nothing -> Nothing


-- | Applies a function to the input stream and discards the
-- result of the function.
alterInput :: ParserM m
            => (B.ByteString -> B.ByteString) -> m ()
alterInput f = work (\s -> Just (() :*: f s))

-- | If @p@ fails it returns @x@, otherwise it returns the result of @p@.
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x

-- | Attempts each option until one succeeds.
choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

-- |Ensure that a parser consumes input when producing a result
-- Causes the initial state of the input stream to be held on to while the
-- parser runs, so use with caution.
checkConsumes :: ParserM m => m a -> m a
checkConsumes parser = do
   x <- B.length <$> peekInput
   res <- parser
   x' <- B.length <$> peekInput
   if x' < x then return res else mzero

class (Functor m, Applicative m, Alternative m, Monad m, MonadPlus m) => ParserM m where
    -- | Applies a parsing function inside the 'ParserM' monad.
    work :: (B.ByteString -> Maybe (ParserState a)) -> m a
    -- | Applies a parsing function, that can return 'Nothing',
    -- inside the 'ParserM' monad.
    maybeWork :: (B.ByteString -> Maybe (ParserState a)) -> m (Maybe a)
    -- | Allows for the inspection of the input that is yet to be parsed.
    peekInput :: m B.ByteString
    -- | Run the parser
    parse :: m a -> B.ByteString -> Maybe (a, B.ByteString)

----- Strict Monad -----
-- | 'parseStrictly' applies the parser functions to a string
-- and checks that each parser produced a result as it goes.
-- The strictness is in the 'ParserM' instance for 'SM'.
parseStrictly :: SM a -> B.ByteString -> Maybe (a, B.ByteString)
parseStrictly (SM f) s = case f s of
  Just (a :*: r) -> Just (a, r)
  _ -> Nothing

-- | ParserState represents the internal state of the parser.  We make it
-- strict and specialize it on ByteString.  This is purely to help GHC
-- optimize.  If performance were not a concern, it could be replaced
-- with @(a, ByteString)@.
data ParserState a = !a :*: !B.ByteString

-- | Convert from a lazy tuple to a strict tuple.
stuple :: (a, B.ByteString) -> ParserState a
stuple (a, b) = a :*: b

-- | 'SM' is the Strict Monad for parsing.
newtype SM a = SM (B.ByteString -> Maybe (ParserState a))

bindSM :: SM a -> (a -> SM b) -> SM b
bindSM (SM m) k = SM $ \s -> case m s of
                             Nothing -> Nothing
                             Just (x :*: s') ->
                               case k x of
                               SM y -> y s'
{-# INLINE bindSM #-}
returnSM :: a -> SM a
returnSM x = SM (\s -> Just (x :*: s))
{-# INLINE returnSM #-}
failSM :: String -> SM a
failSM _ = SM (\_ -> Nothing)
{-# INLINE failSM #-}

instance Monad SM where
    (>>=)  = bindSM
    return = returnSM
    fail   = failSM

instance ParserM SM where
    work f = SM f
    maybeWork f = SM $ \s -> case f s of
                                  Just (x :*: s') -> Just (Just x :*: s')
                                  Nothing -> Just (Nothing :*: s)
    peekInput = SM $ \s -> Just (s :*: s)
    parse = parseStrictly

-- The following instances allow us to use more conventional
-- interfaces provided by other parser libraries. The instances are
-- defined using bindSM, returnSM, and failSM to avoid any infinite,
-- or even unneccessary, recursion of instances between between
-- ParserM and Monad.  Other recursive uses will be fine, such as
-- (<|>) = mplus.
instance MonadPlus SM where
  mzero = failSM ""
  -- | Over using mplus can lead to space leaks.  It's best to push
  -- the use of mplus as far down as possible, because until the the
  -- first parameter completes, we must hold on to the input.
  mplus (SM a) (SM b) = SM $ \s ->
    case a s of
      Nothing -> b s
      r -> r

instance Functor SM where
  fmap f m = m `bindSM` (returnSM . f)

instance Applicative SM where
  pure = returnSM
  a <*> b =
    a `bindSM` \c ->
    b `bindSM` \d ->
    returnSM (c d)

instance Alternative SM where
  empty = failSM ""
  (<|>) = mplus
