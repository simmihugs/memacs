{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parser.Json where

{-

This Part is from https://hasura.io/blog/parser-combinators-walkthrough/

(Added Comments)

-}

import qualified Data.HashMap.Strict as M
import Data.HashMap.Strict
import Data.Char ( toLower )
import Data.List (intercalate)
import Parser.Parser
    ( Parser,
      satisfy,
      try,
      choice,
      many,
      sepBy,
      char,
      digit,
      string,
      spaces,
      symbol,
      between,
      brackets,
      braces,
      some )

import Data.Data (Data)
import Data.Typeable (Typeable)


-- | Haskel Datatype representing JSON Datatypes.
data JValue = JObject (M.HashMap String JValue)
            | JArray  [JValue]
            | JString String
            | JNumber Double
            | JBool   Bool
            | JNull
            deriving (Eq, Data, Typeable)


-- | Creating Show instance for Json values.
instance Show JValue where
  show JNull       = "null"
  show (JBool b  ) = toLower <$> show b
  show (JNumber n) = show n
  show (JString s) = show s
  show (JArray  a) = show a
  show (JObject o) = (\x -> "{" ++ x ++ "}") $ intercalate ", " [show k ++ ": " ++ show v | (k,v) <- M.toList o]


-- | Parsing of JSON file (Ignore whitespace at begining).
json :: Parser JValue
json = spaces >> jsonValue


-- | Parser for JValue.
jsonValue :: Parser JValue
jsonValue = choice [ JObject <$> jsonObject
                   , JArray  <$> jsonArray
                   , JString <$> jsonString
                   , JNumber <$> jsonNumber
                   , JBool   <$> jsonBool
                   , JNull   <$  symbol "null" ]


-- | Parser for JObject.
jsonObject :: Parser (M.HashMap [Char] JValue)
jsonObject = do
  list <- braces $ jsonEntry `sepBy` symbol ","
  return $ M.fromList list
  where
    jsonEntry = do
      k <- jsonString
      symbol ":"
      v <- jsonValue
      return (k,v)


-- | Parser for JArray.
jsonArray :: Parser [JValue]
jsonArray = brackets $ jsonValue `sepBy` symbol ","


-- | Parser for JString.
jsonString :: Parser [Char]
jsonString =
  between (char '"') (char '"') (many jsonChar) <* spaces
  where
    jsonChar = choice [ try $ '\n' <$ string "\\n"
                      , try $ '\t' <$ string "\\t"
                      , try $ '"'  <$ string "\\\""
                      , try $ '\\' <$ string "\\\\"
                      , satisfy (/= '"') ]


-- | Parser for JDouble.
jsonNumber :: Parser Double
jsonNumber = read <$> some digit


-- | Parser for JBool
jsonBool :: Parser Bool
jsonBool = choice [ True  <$ symbol "true"
                  , False <$ symbol "false" ]
