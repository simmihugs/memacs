{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parser.CommandLine where

{- Parser for Commandline input. -}

import qualified Data.HashMap.Strict as M
import Data.Char ( toLower, isAlphaNum )
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
      some,
      run,
      (<|>))

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word
import Data.Maybe
import Monomer


type Operator = String
type Value    = String

-- CMD result will be handled in Eventhandler
newtype CMD = INS (Operator,Value)
            deriving (Show, Eq, Data, Typeable)


cl :: Parser CMD
cl = spaces >> scanCL

scanCL = INS <$> findInstruction

-- findInstruction will find one of the instructions
-- no tree no list
findInstruction :: Parser (Operator,Value)
findInstruction = choice [saveAs, newFile, openFile, showSettings, openFileDialog]
  where
    -- only the word settings
    showSettings = do
      symbol "settings"
      return ("settings","")

    -- only the word newfile
    newFile = do
      symbol "newfile"
      return ("newFile","")

    -- the command `openFile <FILENAME>`
    openFile = do
      string "openFile"
      spaces
      str <- some (satisfy (\x -> isAlphaNum x || x=='.')) <* spaces
      return ("openFile",str)

    -- the command saveAs
    saveAs = do
      string "saveAs"
      spaces
      str <- some (satisfy (\x -> isAlphaNum x || x=='.')) <* spaces
      return ("saveAs",str)

    -- the command openFile
    openFileDialog = do
      string "openFile"
      spaces
      return ("openFile","")

  
