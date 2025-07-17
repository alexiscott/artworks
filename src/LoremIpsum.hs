module LoremIpsum
  ( getRandomParagraphs
  , dummyParagraphs
  , takeRandom
  ) where

import Prelude hiding (div)
import Text.Blaze.Html5 as H
import System.IO (readFile)
import System.Random (randomRIO)
import Data.List.Split (splitOn)
import Data.List (elem)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- Read paragraphs from file and select 4 random ones
getRandomParagraphs :: FilePath -> IO [String]
getRandomParagraphs path = do
    content <- readFile path
    let paragraphs = filter (not . null) (splitOn "\n\n" content)
    if null paragraphs 
        then return ["Lorem ipsum"]  -- Fallback if file is empty
        else takeRandom 4 paragraphs
-- 
-- -- Generate HTML paragraphs from text
-- dummyParagraphs :: H.Html
dummyParagraphs = do
  paragraphs <- getRandomParagraphs "lorem.txt"
  return $ mconcat (Prelude.map (\p -> H.p (H.toHtml p)) paragraphs)

-- Select n distinct random items from a list (guarantees exactly n items)
takeRandom :: Int -> [a] -> IO [a]
takeRandom n xs
    | n <= 0    = return []
    | n >= len  = return xs
    | otherwise = getDistinctIndices n len []
  where
    len = length xs
    getDistinctIndices 0 _ acc = return $ Prelude.map (xs !!) acc
    getDistinctIndices k len acc = do
        idx <- randomRIO (0, len - 1)
        if idx `elem` acc
            then getDistinctIndices k len acc
            else getDistinctIndices (k-1) len (idx:acc)
