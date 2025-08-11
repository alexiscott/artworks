{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes)
import LoremIpsum (dummyParagraphs)
import MakeWorks (Work (..), works, worksJson)
import System.Directory (createDirectoryIfMissing)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Prelude hiding (div)
import Data.List (sortOn)

-- Replace these with the represented artist name.
artistsName :: H.Html
artistsName = "Oliver Twist"


-- Page configuration data type
data PageConfig = PageConfig
  { pageTitle :: H.Html
  , activeNavIndex :: Int
  , regions :: [Region]
  }

data Region
  = NavigationRegion
  | HeaderRegion
  | PreWorkRegion Work
  | ContentRegion (H.Html -> H.Html)
  | WorksGridRegion [Work]
  | ChronologyRegion (H.Html -> H.Html)
  | FooterRegion
-- Navigation items
navItems :: [(String, AttributeValue)]
navItems =
  [ ("Home", "/index.html")
  , ("About", "/about.html")
  , ("Chronology", "/chronology.html")
  , ("Works", "/works.html")
  , ("Search", "/search.html")
  ]

-- Main render function
renderPage :: PageConfig -> H.Html
renderPage config = docTypeHtml $ do
  H.head $ do
    H.title (pageTitle config <> " | " <> artistsName <> " Online Catalogue Raisonné")
    meta ! charset "utf-8"
    link ! rel "stylesheet" ! href "styles.css"
    link ! rel "stylesheet" ! href "grid.css"
    link ! rel "stylesheet" ! href "works.css" -- Possibly make specific.
    -- when (activeNavIndex config == 4) $ do
    --   script ! type_ "application/javascript" $ "works=" <> toHtml worksJson
    --   script ! src "lunr.js.js" ! type_ "application/javascript" $ ""
    --   script ! src "search.js" ! type_ "application/javascript" $ ""
  body ! class_ "page" $ do
    mapM_ renderRegion (regions config)
    where
      renderRegion :: Region -> H.Html
      renderRegion region = case region of
        NavigationRegion ->
          nav ! class_ "navigation-region" $ navigation (activeNavIndex config)

        HeaderRegion ->
          header ! class_ "header-region" $ do
            p $ do
              a ! href "/" ! alt "Back works"
                ! A.title "Back to works"
                ! class_ "arrow-link" $ "⬅ Back to works"

        PreWorkRegion work ->
          H.div ! class_ "pre-work-region" $ do
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

        ContentRegion contentFn ->
           H.div $ contentFn  $ H.strong "We have no content here yet."

        WorksGridRegion worksList ->
          H.div ! class_ "works-grid-region" $
            mapM_ renderWorkThumbnail (sortWorks worksList)

        ChronologyRegion contentFn ->
          H.div ! class_ "chronology-region" $ contentFn "lorem"

        FooterRegion ->
          footer ! class_ "footer-region" $ do
            p ("© 2025 " <> artistsName <> " Foundation")
            when (activeNavIndex config == 4) $ do
              script ! type_ "application/javascript" $ "works=" <> toHtml worksJson
              script ! src "lunr.js.js" ! type_ "application/javascript" $ ""
              script ! src "search.js" ! type_ "application/javascript" $ ""


-- Navigation component
navigation :: Int -> Html
navigation activeIndex =
  ul ! class_ "navigation" $ mconcat $
    zipWith renderItem [0..] navItems
  where
    renderItem :: Int -> (String, AttributeValue) -> Html
    renderItem idx (lab, url) =
      (if idx == activeIndex then li ! class_ "active" else li) $
        a ! href url $ toHtml lab

-- Work-related components
renderWorkThumbnail :: Work -> H.Html
renderWorkThumbnail work =
  H.div ! class_ "work-thumbnail" $ do
    a ! href (toValue $ "/" ++ show (catalogueNumber work) ++ ".html") $ do
      workThumbnailImage work
    renderWorkCaption work

workThumbnailImage :: Work -> H.Html
workThumbnailImage work =
  img
    ! src (toValue $ "images/" ++ show (catalogueNumber work) ++ ".jpg")
    ! alt (toValue $ "Artwork: " ++ workTitle work)
    ! class_ "work-thumbnail-img"

renderWorkCaption :: Work -> H.Html
renderWorkCaption work =
  H.div ! class_ "work-caption" $ do
    a ! href (toValue $ "/" ++ show (catalogueNumber work) ++ ".html") $ do
      H.div ! class_ "work-caption-title" $ toHtml (workTitle work)
      H.div ! class_ "work-caption-details" $ do
        toHtml (date work)
        ", "
        toHtml (show (catalogueNumber work))

-- Sort works by catalogue number
sortWorks :: [Work] -> [Work]
sortWorks = sortOn catalogueNumber

-- Main content for work pages
mainContent :: Work -> H.Html -> H.Html
mainContent work contentZ = do
  H.div ! class_ "grid" $ do
    -- First column: Image section
    H.div ! class_ "grid-item" $ do
      H.div ! class_ "image-container" $
        workImage work
    -- Second column: Text section
    H.div ! class_ "grid-item text-section" $ do
      contentZ

workImage :: Work -> H.Html
workImage work =
  img
    ! src (toValue $ "images/" ++ show (catalogueNumber work) ++ ".jpg")
    ! alt (toValue $ "Artwork: " ++ workTitle work)
    ! class_ "artwork-image"

-- Page configurations
homePageConfig :: PageConfig
homePageConfig = PageConfig
  { pageTitle = "Home"
  , activeNavIndex = 0
  , regions =
      [ NavigationRegion
      , ContentRegion (\_ ->
          H.div ! class_ "page-home" $ do
            H.div $ do
              h1 artistsName
              h2 "Online catalogue Raisonné"
              img ! src "images/sheet_of_studies_recto_1991.217.2.a.jpg" ! width "260px")
      , FooterRegion
      ]
  }

aboutPageConfig :: H.Html -> PageConfig
aboutPageConfig lor = PageConfig
  { pageTitle = "About the Online Catalogue Raisonné"
  , activeNavIndex = 1
  , regions =
      [ NavigationRegion
      , ContentRegion (\c ->
          H.div ! class_ "flowing-grid text-section" $ do
            H.div $ do
              p $ do
                "About " <> artistsName <> "'s artwork." <> lor
              c)
      , FooterRegion
      ]
  }

chronologyPageConfig :: PageConfig
chronologyPageConfig = PageConfig
  { pageTitle = "Chronology"
  , activeNavIndex = 2
  , regions =
      [ NavigationRegion
      , ChronologyRegion (\c ->
          H.div ! class_ "chronology-container" $ do
            H.div ! class_ "chronology-intro" $ do
              p "This chronology presents the key events in" <> artistsName <> "'s life and career."
            H.div ! class_ "chronology-content" $ c)
      , FooterRegion
      ]
  }

worksPageConfig :: [Work] -> PageConfig
worksPageConfig worksList = PageConfig
  { pageTitle = "Works"
  , activeNavIndex = 3
  , regions =
      [ NavigationRegion
      , WorksGridRegion worksList
      , FooterRegion
      ]
  }

searchPageConfig :: PageConfig
searchPageConfig = PageConfig
  { pageTitle = "Search the Collection"
  , activeNavIndex = 4
  , regions =
      [ NavigationRegion
      , ContentRegion (\_ ->
          H.div ! class_ "search-works" $ do
            h2 "Search Works"
            H.input
              ! type_ "text"
              ! A.id "searchBox"
              ! placeholder "Search by title, date, medium..."
              ! A.style "width: 100%; padding: 8px;"
            H.div ! A.id "results" $ ""
          )
      , FooterRegion
      ]
  }

workPageConfig :: Work -> PageConfig
workPageConfig work = PageConfig
  { pageTitle = "workTitle work" -- AIS make dynamic.
  , activeNavIndex = 3  -- Works is active when viewing individual works
  , regions =
      [ NavigationRegion
      , HeaderRegion
      , PreWorkRegion work
      , ContentRegion (mainContent work)
      , FooterRegion
      ]
  }

-- Main function
main :: IO ()
main = do
  createDirectoryIfMissing True "output"
  lorem <- dummyParagraphs
  let validWorks = catMaybes works

  -- Home/Index
  BL.writeFile "output/index.html" $
    renderHtml (renderPage homePageConfig)

  -- About
  BL.writeFile "output/about.html" $
    renderHtml (renderPage (aboutPageConfig lorem))

  -- Chronology
  BL.writeFile "output/chronology.html" $
    renderHtml (renderPage chronologyPageConfig)

  -- Works
  BL.writeFile "output/works.html" $
    renderHtml (renderPage (worksPageConfig validWorks))

  -- Search
  BL.writeFile "output/search.html" $
    renderHtml (renderPage searchPageConfig)

  -- Individual work pages
  forM_ validWorks $ \work -> do
    let filename = "output/" ++ show (catalogueNumber work) ++ ".html"
    BL.writeFile filename $ renderHtml $ renderPage (workPageConfig work)
