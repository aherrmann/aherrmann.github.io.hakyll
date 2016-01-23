--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.DateTime (formatDateTime, getCurrentTime)
import           Data.List (isInfixOf)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import           Hakyll
import           System.FilePath.Posix (takeDirectory, splitFileName)

-- TODO: Decide on one of these options.
--
--        * Use Twitter Bootstrap. (Optionally v4)
--        * Use Foundation. (There is a lucid-foundation package)
--        * Use PureCSS.
--        * Use Bourbon + Neat, or Compass + Susy.


--------------------------------------------------------------------------------
main :: IO ()
main = do
  currYear <- formatDateTime "%Y" <$> getCurrentTime
  let pageCtx = constField "currYear" currYear `mappend` defaultContext
      postCtx = dateField "date" "%B %e, %Y"              `mappend`
                constField "teaser" "TODO: Make teaser"   `mappend`
                pageCtx

  hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls
          >>= removeIndexHtml

    match "posts/*" $ do
      route   postRoute
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls
          >>= removeIndexHtml

    create ["archive.html"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let archiveCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Archives"            `mappend`
                  pageCtx

          makeItem ""
              >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls
              >>= removeIndexHtml

    match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          let indexCtx =
                  listField "posts" postCtx (return posts) `mappend`
                  constField "title" "Home"                `mappend`
                  pageCtx

          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls
              >>= removeIndexHtml

    create [ "atom.xml" ] $ do
      route idRoute
      compile $ do
          posts <- fmap (take 10) . recentFirst =<< loadAll "posts/*"
          let feedConfig = FeedConfiguration
                  { feedTitle       = "Programming Blog"
                  , feedDescription = ""
                  , feedAuthorName  = "Andreas Herrmann"
                  , feedAuthorEmail = "andreash87@gmx.ch"
                  , feedRoot        = "http://aherrmann.github.io"
                  }
              feedCtx =
                  constField "description" "" `mappend`
                  postCtx
          renderAtom feedConfig feedCtx posts
              >>= removeAllIndexHtml

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postRoute :: Routes
postRoute =
    -- TODO: Consider to store articles in a path that defines the category.
    --       Then use the technique described under the following link to
    --       format the route.
    --       https://biosphere.cc/software-engineering/beautify-hakyll-post-urls-removing-extension-timestamp/
    metadataRoute makeCategoryPath `composeRoutes`
    postDateRoute `composeRoutes`
    setExtension "html" `composeRoutes`
    asIndexRoute "html"


--------------------------------------------------------------------------------
makeCategoryPath :: Metadata -> Routes
makeCategoryPath md =
    gsubRoute "posts/" $ const . (++ "/") $
    fromMaybe (error "Posts: Post without category!") $
    M.lookup "category" md


--------------------------------------------------------------------------------
postDateRoute :: Routes
postDateRoute =
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")


--------------------------------------------------------------------------------
asIndexRoute :: String -> Routes
asIndexRoute extension =
    gsubRoute ('.':extension) $ const ("/index." ++ extension)


--------------------------------------------------------------------------------
-- | Replace url of the form foo/bar/index.html by foo/bar.
removeIndexHtml :: Item String -> Compiler (Item String)
removeIndexHtml item = return $ fmap (withUrls removeIndexStr) item
    where
        removeIndexStr :: String -> String
        removeIndexStr url =
            case splitFileName url of
                (dir, "index.html") | isLocal dir -> takeDirectory dir
                _                                 -> url
        isLocal :: String -> Bool
        isLocal uri        = not ("://" `isInfixOf` uri)

-- | Replace url of the form *foo/bar/index.html by *foo/bar.
--   This includes external urls.
removeAllIndexHtml :: Item String -> Compiler (Item String)
removeAllIndexHtml item = return $ fmap (withUrls removeIndexStr) item
    where
        removeIndexStr :: String -> String
        removeIndexStr url =
            case splitFileName url of
                (dir, "index.html") -> takeDirectory dir
                _                   -> url
