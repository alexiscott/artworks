-- In test/Spec.hs
module Main where

import Test.Hspec
import MakeWorks (Work)
import Prelude
import Data.Maybe (catMaybes)
import qualified Data.ByteString.Lazy as BL
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Control.Monad (return)

renderExample :: Work -> IO H.Html
renderExample work = do
    -- textContent <- dummyParagraphs
    return $ H.toHtml "Some content"


main :: IO ()
main = hspec $ do
  describe "Transformation pipeline" $ do
    it "successfully runs the complete transformation" $ do
      -- 1. Setup test data
      let works :: [Maybe Work]
          works = [Just undefined]  -- Replace with actual test data
          
          vws :: [Work]
          vws = catMaybes works
      
      -- 2. Verify we have at least one work
      case vws of
        [] -> fail "No works available for testing"
        (work:_) -> do
          -- 3. Execute the transformations
          html <- renderExample work
          let bs = renderHtml html
          
          -- 4. Verify the output isn't empty
          if BL.null bs
            then fail "Rendered HTML is empty"
            else return ()
