module Parser.Parser where

import Control.Applicative (liftA2)
import Data.Char
import Data.Functor
import Prelude hiding (any)

{-

Based on tutorial found at 

https://hasura.io/blog/parser-combinators-walkthrough/

but different type of parser -> Maybe instead of ErrorType

-}



-- | Parser for generic type a.
-- | Given a String the Parser returns maybe a tuple (a, restString)
newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
  } 

instance Functor Parser where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f pa = Parser $ \str -> case runParser pa str of
    Just (a, restString) -> Just (f a, restString)
    Nothing              -> Nothing

-- | Applicative helps us to chain Parsers together to a
-- | new and better Parser.
instance Applicative Parser where
  -- pure :: a -> f a
  pure c = Parser $ \str -> Just (c, str)
  -- `apply` 
  -- (<*>) :: f (a -> b) -> f a -> f b
  pa1 <*> pa2 = Parser $ \str -> case runParser pa1 str of
    Just (f, s') -> case runParser pa2 s' of
      Just (f'', s'') -> Just (f f'', s'')
      Nothing -> Nothing
    Nothing      -> Nothing

-- | Being a Monad makes bind and therefore do
-- | accessible to Parser.
instance Monad Parser where
  -- (>>=) :: m a -> (a -> m b) -> m b
  pa >>= f = Parser $ \s -> case runParser pa s of
    Just (c, restString) -> runParser (f c) restString
    Nothing -> Nothing


-- | Run a parser on a inputString.
run :: Parser a -> String -> Maybe (a, String)
run p = runParser (p <* eof)


-- | End of `file`.
eof :: Parser ()
eof = Parser $ \s -> case s of
  [] -> Just ((),"")
  _  -> Nothing

-- | Parse any kind of single character.
any :: Parser Char
any = Parser $ \s -> case s of
  []     -> Nothing
  (x:xs) -> Just (x, xs)

-- | Check wether a character satifies a predicate. If yes
-- | parse it if not fail parsing.
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = try $ do
  c <- any
  if predicate c
    then pure c
    else char ' '

-- | Try Parser, if it fails it restores previous state
try :: Parser a -> Parser a
try p = Parser $ \s -> maybe Nothing Just (runParser p s)

-- | Combine Parsers.
-- | Parse either first Parser or in case of failure second Parser.
(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = Parser $ \s -> case runParser p1 s of
  Nothing -> runParser p2 s
  success -> success
  
--   where noMatch = parseError description "no match"
-- | Parse first succeding Parser from List of Parsers.
choice :: [Parser a] -> Parser a
choice = foldr1 (<|>) 

-- | Parse repeatedly (e.g. multiple chars -> string)
-- | Parse 0..n time.
many :: Parser a -> Parser [a]
many p = some p <|> pure []

-- | Parse 1..n time.
some :: Parser a -> Parser [a]
some p = do
  first <- p
  rest  <- many p
  return (first:rest)

-- | Parse repeatedly with separator between parsers.
sepBy :: Parser a -> Parser s -> Parser [a]
sepBy p s = sepBy' p s <|> pure []
  where
    sepBy' p s = do
      first <- p
      rest  <- many (s >> p)
      return (first:rest)


-- | Parse Characters.
-- | Parses one Character.
char :: Char -> Parser Char
char c = Parser char'
  where
    char' [] = Nothing
    char' (x:xs)
      | x==c      = Just (c,xs)
      | otherwise = Nothing


-- | Parses one Spacecharacter.
--space :: Parser Char
space  = choice [char ' ', char '\n', char '\r', char '\t']

-- | Parses one digit.
digit :: Parser Char
digit = Parser digit'
  where
    digit' []     = Nothing
    digit' (x:xs) = if isDigit x
                    then Just (x, xs)
                    else Nothing 

-- | Parse Character based entities.
-- | Parse one String.
string = traverse char

-- | Parse 0..n space Characters.
spaces = many space

-- | Parses a Symbol - String with whitespace on the left.
symbol s = string s <* spaces



-- | Parse between surroundings.
-- | Parses something between symbols.
between :: Applicative f => f a1 -> f b -> f a2 -> f a2
between o c p = o *> p <* c

-- | Parses something between [].
brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

-- | Parses something between {}.
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Parses something between ().
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
