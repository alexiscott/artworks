{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE OverloadedStrings #-}

module MakeWorks (Work(..), works, worksJson) where

import Data.List (isPrefixOf)
import Text.Read (readMaybe)
import Data.Char (isSpace)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as B

instance ToJSON Work

-- Define the Work data type
data Work = Work
  { catalogueNumber :: Int
  , workTitle      :: String
  , date           :: String
  , medium         :: String
  , dimensions     :: String
  , signature      :: String
  , location       :: String
  , description    :: String
  , examined       :: String
  , exhibitions    :: String
  , literature     :: Maybe String
  } deriving (Show, Generic)

-- Parse a string containing an Org-mode table into a Work object
parseWork :: String -> Maybe Work
parseWork s = do
  let rows = lines s
      tableRows = [ splitRow row | row <- rows, isTableRow row ]
      validRows = filter (\cells -> case cells of
                              (x:_) -> length cells >= 2 && not (null x)
                              [] -> False) tableRows
      pairs = [ (trim a, trim b) | (a:b:_) <- validRows ]

  -- Required fields
  catNumStr <- lookup "A unique Catalogue Raisonné number" pairs
  catNum    <- readMaybe catNumStr  -- Convert to Int
  titleVal  <- lookup "The title of the work" pairs
  dateVal   <- lookup "Date" pairs
  mediumVal <- lookup "Medium" pairs
  dimVal    <- lookup "Dimensions" pairs
  sigVal    <- lookup "Signature" pairs
  locVal    <- lookup "Location" pairs
  descVal   <- lookup "Description" pairs
  examVal   <- lookup "Examined" pairs
  exhVal    <- lookup "Exhibitions" pairs

  -- Optional field
  let litVal = lookup "Literature" pairs

  return $ Work catNum titleVal dateVal mediumVal dimVal sigVal locVal descVal examVal exhVal litVal
  where
    isTableRow :: String -> Bool
    isTableRow row = "|" `isPrefixOf` row

    splitRow :: String -> [String]
    splitRow row = map trim $ splitOn '|' (init $ drop 1 row)

    trim :: String -> String
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

    splitOn :: Char -> String -> [String]
    splitOn _ [] = []
    splitOn sep sz = chunk : splitOn sep rest
      where
        (chunk, rest') = break (== sep) sz
        rest = case rest' of
                []      -> []
                (_:xs) -> xs

table1 :: String
table1 = unlines
  [ "| A unique Catalogue Raisonné number | 1001                                                                       | Int |"
  , "| The title of the work              | Abstract Composition No. 5                                                       | Str |"
  , "| Date                               | 1947                                                                             | Str |"
  , "| Medium                             | Oil and sand on masonite                                                         | Str |"
  , "| Dimensions                         | 76.2 x 61 cm /30 x 24 in                                                        | Str |"
  , "| Signature                          | Signed lower left 'BLOGGS 47'                                                    | Str |"
  , "| Location                           | Metropolitan Museum of Modern Art, New York                                      | Str |"
  , "| Description                        | Features Bloggs' characteristic textured surface with ochre and ultramarine tones | Str |"
  , "| Examined                           | 2019                                                                             | Str |"
  , "| Exhibitions                        | Guggenheim Museum, New York, 'Post-War Abstracts', 1952                         | Str |"
  ]

table2 :: String
table2 = unlines
  [ "| A unique Catalogue Raisonné number | 1002                                                                       | Int |"
  , "| The title of the work              | Urban Fragment                                                                    | Str |"
  , "| Date                               | 1948                                                                             | Str |"
  , "| Medium                             | Mixed media on board                                                             | Str |"
  , "| Dimensions                         | 91.4 x 121.9 cm /36 x 48 in                                                      | Str |"
  , "| Signature                          | Signed and dated verso 'J. Bloggs 48'                                            | Str |"
  , "| Location                           | Tate Modern, London                                                              | Str |"
  , "| Description                        | Early experiment incorporating newspaper clippings into abstract composition     | Str |"
  , "| Examined                           | 2020                                                                             | Str |"
  , "| Exhibitions                        | Whitechapel Gallery, London, 'British Constructivists', 1949                     | Str |"
  , "| Literature                         | Clement Greenberg, 'Joe Bloggs: The Early Years', Artforum Press, 1962           | Str |"
  ]

-- Parse the tables.
parsedWork1 :: Maybe Work
parsedWork1 = parseWork table1

parsedWork2 :: Maybe Work
parsedWork2 = parseWork table2

works :: [Maybe Work]
works = [parsedWork1, parsedWork2]

worksJson = encode works
