{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parser.Hex 
  (
    module Parser.Hex,
    module Parser.Parser
  )
where


import qualified Data.HashMap.Strict as M
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
      some,
      run )

import Data.Data (Data)
import Data.Typeable (Typeable)
import Data.Word
import Data.Maybe
import Monomer

{-
Hex parser determines int value of hexadicimel numer
-}


data Hex = HexString Int
         | HexChar Int
         | HexFirstChar Int
         deriving (Show, Eq, Data, Typeable)

-- extract inner value from Hex
unpack :: Hex -> Int
unpack (HexString int)    = int
unpack (HexChar int)      = int
unpack (HexFirstChar int) = int


hex :: Parser Hex
hex = spaces >> scanHex

-- checks if a value is hex color code  
isColorHex :: String -> Bool
isColorHex str = do
  let parsed = run hex str
  case parsed of
    Nothing -> False
    Just (value,_) -> do
      let val = unpack value
      0<=val && val<=16777215


-- translate string to rgb
toRGB :: String -> (Word8, Word8, Word8)
toRGB str = do
  let (Color r g b _) = rgbHex str
  (fromIntegral r, fromIntegral g, fromIntegral b)


-- translate rgb to String
toHex :: Word8 -> String
toHex v = do
  let v' = fromIntegral v
  toHex' v' ""
  where
    showC :: Char -> [Char]
    showC c = [c]

    hexMap = zip [0..pred (length list)] list
      where
        list = map show [0..9] ++ map showC ['a'..'f']
    toHexString x = fromJust $ lookup x hexMap 

    -- | Calculate hexstring from number
    toHex' :: Int -> String -> String
    toHex' v acc
      | v < 16 = toHexString v ++ acc
      | otherwise = do
          let quotient  = v `div` 16
          let remainder = v `mod` 16
          toHex' quotient (toHexString remainder ++ acc)

-- translate rgb to String
fromRGB :: (Word8, Word8, Word8) -> String
fromRGB (r, g, b) = do
  let red   = (\x -> if length x == 1 then "0"++x else x) $ toHex r
  let green = (\x -> if length x == 1 then "0"++x else x) $ toHex g
  let blue  = (\x -> if length x == 1 then "0"++x else x) $ toHex b
  '#' : red ++ green ++ blue


-- Parser runs in order
-- First find # symbol
scanHex :: Parser Hex
scanHex = HexFirstChar <$> hexFirstChar

hexFirstChar :: Parser Int
hexFirstChar = char '#' >> hexString 
-- then calls the hexString parser

hexString :: Parser Int
hexString =
  -- after collecting the chars->Int
  -- calculate decimal value of each digit
  -- sum them up -> result
  fmap countHexNumbers $ (many hexChar) <* spaces  
  where
    countHexNumbers x = cHN x 0
    cHN [] acc = acc
    cHN list@(x:xs) acc = cHN xs (acc + x * (16 ^ pred (length list)))


-- | excepts onlye valid hex strings and translates into
-- | decimal
hexChar :: Parser Int
hexChar = choice [ 0  <$ symbol "0"
                 , 1  <$ symbol "1"
                 , 2  <$ symbol "2"
                 , 3  <$ symbol "3"
                 , 4  <$ symbol "4"
                 , 5  <$ symbol "5"
                 , 6  <$ symbol "6"
                 , 7  <$ symbol "7"
                 , 8  <$ symbol "8"
                 , 9  <$ symbol "9"
                 , 10  <$ symbol "a"
                 , 11  <$ symbol "b"
                 , 12  <$ symbol "c"
                 , 13  <$ symbol "d"
                 , 14  <$ symbol "e"
                 , 15  <$ symbol "f"
                 ]

