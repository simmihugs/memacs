module Parser.Open where

import qualified Data.HashMap.Strict as M
import Data.Char ( toLower, isAlphaNum )
import Data.List (intercalate)
import Parser.Parser ( Parser
              , satisfy
              , choice
              , some
              , many
              , try
              , string
              , spaces
              , between
              , char
              , (<|>)
              )

type VarName = String

-- | Datatype for opening files/functions/lists
-- | VarName allows for the option in quasiquoter to insert
-- | variable from outside
data Open = FileName (Either String VarName)
          | FileFunc (Either String VarName)
          | OpenList [Open]
          deriving (Eq, Show)


open :: Parser Open
open = spaces >> openValue


openValue :: Parser Open
openValue = choice [ OpenList <$> list
                   , FileFunc <$> fileFunc
                   , FileName <$> fileName ]

-- Create list of filename and filefunc
list :: Parser [Open]
list = do
  f <- fileName
  spaces
  t <- fileFunc <* spaces
  return [FileName f, FileFunc t]
  <|> do
    f <- fileFunc
    spaces
    t <- fileName <* spaces
    return [FileName f, FileFunc t]


-- Parse for filename an either variable or `filename`
fileName :: Parser (Either String VarName)
fileName = f1 <|> f2
  where
    f1 :: Parser (Either String VarName)
    f1 = do
      f <- string "fileName"
        >> spaces
        >> string ":="
        >> spaces
        >> char '$'      
        >> some (satisfy isAlphaNum) <* spaces
      return $ Right f

    f2 :: Parser (Either String VarName)
    f2 = do
      f <- string "fileName"
        >> spaces
        >> string ":="
        >> spaces
        >> some (satisfy isAlphaNum) <* spaces
      return $ Left f


-- Parses for fileFunc and either value or variable (inserted in quasiquoter)
fileFunc :: Parser (Either String VarName)
fileFunc = f1 <|> f2
  where
    f1 = do
      f <- string "fileType"
           >> spaces
           >> string ":="
           >> spaces
           >> some (satisfy isAlphaNum) <* spaces
      return $ Left f
    f2 = do
      f <- string "fileType"
           >> spaces
           >> string ":="
           >> spaces
           >> char '$'
           >> some (satisfy isAlphaNum) <* spaces
      return $ Right f

  
