{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import LoremIpsum (dummyParagraphs)
import MakeWorks (Work (..), works)
import System.Directory (createDirectoryIfMissing)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Prelude hiding (div)
import Data.List (sortOn)

navigation :: Int -> Html
navigation activeIndex =
  nav $ ul ! class_ "navigation" $ mconcat $
    zipWith renderItem [0..] navItems
  where
    navItems :: [(String, AttributeValue)]
    navItems =
      [ ("Home", "/index.html")
      , ("About", "/about.html")
      , ("Chronology", "/chronology.html")
      , ("Works", "/works.html")
      , ("Search", "/search.html")
      ]

    renderItem :: Int -> (String, AttributeValue) -> Html
    renderItem idx (label, url) =
      (if idx == activeIndex then li ! class_ "active" else li) $
        a ! href url $ toHtml label

mainContent :: Work -> H.Html -> H.Html
mainContent work lorem = do
  H.div ! class_ "grid" $ do
    -- First column: Image section
    H.div ! class_ "grid-item" $ do
      H.div ! class_ "image-container" $
        workImage work
    -- Second column: Text section
    H.div ! class_ "grid-item text-section" $ do
      lorem

workImage :: Work -> H.Html
workImage work =
  img
    ! src (toValue $ "images/" ++ show (catalogueNumber work) ++ ".jpg")
    ! alt (toValue $ "Artwork: " ++ workTitle work)
    ! class_ "artwork-image"

renderWorkPage :: Work -> H.Html -> H.Html
renderWorkPage work mc = docTypeHtml $ do
  H.head $ do
    H.title $ toHtml (workTitle work <> " | Joe Bloggs Online Catalogue Raisonné")
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
  body ! class_ "work-page" $ do
    navigation 2
    header $ do
      p $ do
        a ! href "/" ! alt "Back works"
          ! A.title "Back to works"
          ! class_ "arrow-link" $ "⬅ Back to works"
    H.div ! class_ "pre-work" $ do
      h2 ! class_ "as_h3 work-catnum" $ toHtml (catalogueNumber work)
      h1 ! class_ "as_h3 work-title" $ toHtml (workTitle work)
      h2 ! class_ "as_h3 work-date" $ toHtml (date work)
      p ! class_ "work-medium" $ toHtml (medium work)
      p ! class_ "work-size" $ toHtml (dimensions work)
      p ! class_ "work-signature" $ toHtml (signature work)
      p ! class_ "work-location" $ toHtml (location work)
      p ! class_ "work-extra" $ do
        a ! href "/" $ "Provenance"
        ", "
        a ! href "/" $ "Exhibitions"
    mc
    footer $ p "© 2025 Joe Bloggs Foundation"

renderHomepage :: H.Html
renderHomepage = docTypeHtml $ do
  H.head $ do
    H.title "Homepage | Joe Bloggs Online Catalogue Raisonné"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
  body $ do
    navigation 0
    H.div ! class_ "page-home" $ do
      H.div $ do
        h1 "Joe Bloggs"
        h2 "Online catalogue Raisonné"
        img ! src "images/118.jpg" ! width "260px"
    footer $ p "© 2025 Joe Bloggs Foundation"

renderAbout :: H.Html -> H.Html -> H.Html
renderAbout pageTitle c = docTypeHtml $ do
  H.head $ do
    H.title "Joe Bloggs Online Catalogue Raisonné"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
  body $ do
    navigation 1
    header $ h1 $ toHtml pageTitle
    H.div ! class_ "flowing-grid text-section" $ do
      H.div $ do
        p $ do
          "About Joe's artwork."
        c

    footer $ p "© 2025 Joe Bloggs Foundation"

renderChronology :: H.Html -> H.Html
renderChronology content = docTypeHtml $ do
  H.head $ do
    H.title "Chronology | Joe Bloggs Online Catalogue Raisonné"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
  body $ do
    navigation 2
    header $ h1 "Chronology 1913-1989"
    H.div ! class_ "chronology-container" $ do
      H.div ! class_ "chronology-intro" $ do
        p "This chronology presents the key events in Joe Bloggs's life and career."
      H.div ! class_ "chronology-content" $ do
        content
    footer $ p "© 2025 Joe Bloggs Foundation"

renderSimpleLayout :: H.Html -> H.Html -> H.Html
renderSimpleLayout pageTitle c = docTypeHtml $ do
  H.head $ do
    H.title "Joe Bloggs Online Catalogue Raisonné"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
  body $ do
    navigation 3
    header $ h1 $ toHtml pageTitle
    H.div ! class_ "flowing-grid text-section" $ do
      H.div $ do
        c
    footer $ p "© 2025 Joe Bloggs Foundation"




    {-# LANGUAGE OverloadedStrings #-}

-- ... (keep all existing imports and other code)

renderWorksPage :: [Work] -> H.Html
renderWorksPage allWorks = docTypeHtml $ do
  H.head $ do
    H.title "Works | Joe Bloggs Online Catalogue Raisonné"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
    link ! rel "stylesheet" ! href "works.css"  -- Additional styling for works page
  body ! class_ "works-page" $ do
    navigation 3
    header $ h1 "Oil Paintings"
    H.div ! class_ "works-container" $ do
      H.div ! class_ "works-grid" $
        mapM_ renderWorkThumbnail (sortWorks allWorks)
    footer $ p "© 2025 Joe Bloggs Foundation"

-- Sort works by catalogue number
sortWorks :: [Work] -> [Work]
sortWorks = sortOn catalogueNumber

-- Render a single work thumbnail with caption
renderWorkThumbnail :: Work -> H.Html
renderWorkThumbnail work =
  H.div ! class_ "work-thumbnail" $ do
    a ! href (toValue $ "/" ++ show (catalogueNumber work) ++ ".html") $ do
      workThumbnailImage work
    renderWorkCaption work

-- Render the thumbnail image
workThumbnailImage :: Work -> H.Html
workThumbnailImage work =
  img
    ! src (toValue $ "images/" ++ show (catalogueNumber work) ++ ".jpg")
    ! alt (toValue $ "Artwork: " ++ workTitle work)
    ! class_ "work-thumbnail-img"

-- Render the caption under each work
renderWorkCaption :: Work -> H.Html
renderWorkCaption work =
  H.div ! class_ "work-caption" $ do
    a ! href (toValue $ "/" ++ show (catalogueNumber work) ++ ".html") $ do
      H.div ! class_ "work-caption-title" $ toHtml (workTitle work)
      H.div ! class_ "work-caption-details" $ do
        toHtml (date work)
        ", "
        toHtml (show (catalogueNumber work))

-- Update the main function to use renderWorksPage
main :: IO ()
main = do
  createDirectoryIfMissing True "output"
  lorem <- dummyParagraphs

  -- Home/Index
  BL.writeFile "output/index.html" $
    renderHtml renderHomepage

  -- About
  BL.writeFile "output/about.html" $
    renderHtml (renderAbout "About the Online Joe Bloggs Catalogue Raisonné" lorem)

  -- Chronology
  BL.writeFile "output/chronology.html" $
    renderHtml (renderChronology lorem)

  -- Works - now using renderWorksPage with the sorted works list
  let validWorks = catMaybes works
  BL.writeFile "output/works.html" $
    renderHtml (renderWorksPage validWorks)

  -- Search
  BL.writeFile "output/search.html" $
    renderHtml (renderSimpleLayout "Search the Collection" lorem)

  -- Individual work pages
  forM_ validWorks $ \work -> do
    let filename = "output/" ++ show (catalogueNumber work) ++ ".html"
    let mc = mainContent work lorem
    BL.writeFile filename $ renderHtml $ renderWorkPage work mc
