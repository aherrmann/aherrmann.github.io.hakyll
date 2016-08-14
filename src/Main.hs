--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Char (isSpace)
import           Data.DateTime (formatDateTime, getCurrentTime)
import           Data.List (intercalate, isInfixOf)
import           Data.List.Split (split, condense, whenElt)
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid (mappend)
import qualified Data.Set as S
import qualified Text.Pandoc.Options as PO
import qualified Text.HTML.TagSoup as TS
import           Text.Hyphenation (english_US, hyphenate)
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
  let pageRoot = "http://aherrmann.github.io"
      pageCtx =
          constField "pageRoot" pageRoot           `mappend`
          constField "currYear" currYear           `mappend`
          defaultContext
      postCtx =
          directoryUrlField "url"                  `mappend`
          dateField "date" "%B %e, %Y"             `mappend`
          pageCtx
      teasCtx =
          teaserField "teaser" "content"           `mappend`
          postCtx
      customPandocCompiler =
          let mathExtensions = [ PO.Ext_tex_math_dollars
                               , PO.Ext_tex_math_double_backslash
                               , PO.Ext_latex_macros ]
              defaultExtensions =
                  PO.writerExtensions defaultHakyllWriterOptions
              newExtensions =
                  foldr S.insert defaultExtensions mathExtensions
              writerOptions = defaultHakyllWriterOptions
                { PO.writerExtensions = newExtensions
                , PO.writerHTMLMathMethod = PO.MathJax ""
                , PO.writerHtml5 = True
                }
          in pandocCompilerWith defaultHakyllReaderOptions writerOptions

  hakyll $ do
    match "images/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "fonts/*" $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*.hs" $ do
      route   $ setExtension "css"
      compile $ getResourceString
          >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    match "posts/*" $ do
      route   postRoute
      compile $ pandocCompiler
          >>= hyphenateItem
          >>= loadAndApplyTemplate "templates/post-content.html" postCtx
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html"         postCtx
          >>= loadAndApplyTemplate "templates/default.html"      postCtx
          >>= relativizeUrls

    match "archive.html" $ do
      route   $ asIndexRoute "html"
      compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let archiveCtx =
                  directoryUrlField "url"                  `mappend`
                  listField "posts" teasCtx (return posts) `mappend`
                  pageCtx

          getResourceBody
              >>= applyAsTemplate archiveCtx
              >>= loadAndApplyTemplate "templates/default.html" archiveCtx
              >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
          let indexCtx =
                  listField "posts" teasCtx (return posts) `mappend`
                  pageCtx

          getResourceBody
              >>= applyAsTemplate indexCtx
              >>= loadAndApplyTemplate "templates/default.html" indexCtx
              >>= relativizeUrls

    create [ "atom.xml" ] $ do
      route idRoute
      compile $ do
          posts <- fmap (take 10) . recentFirst             =<<
                       (return . getTeaserContents)         =<<
                       loadAllSnapshots "posts/*" "content"
          let feedConfig = FeedConfiguration
                  { feedTitle       = "Programming Blog"
                  , feedDescription = ""
                  , feedAuthorName  = "Andreas Herrmann"
                  , feedAuthorEmail = "andreash87@gmx.ch"
                  , feedRoot        = pageRoot
                  }
              feedCtx =
                  teasCtx                                  `mappend`
                  bodyField "description"

          renderAtom feedConfig feedCtx posts

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
    lookupString "category" md


--------------------------------------------------------------------------------
postDateRoute :: Routes
postDateRoute =
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")


--------------------------------------------------------------------------------
asIndexRoute :: String -> Routes
asIndexRoute extension =
    gsubRoute ('.':extension) $ const ("/index." ++ extension)


--------------------------------------------------------------------------------
-- | Transform a URL field to one that crops the /index.html in the end.
directoryUrlField :: String -> Context a
directoryUrlField key = mapContext removeIndexStr (urlField "url")
    where
        removeIndexStr :: String -> String
        removeIndexStr url =
            case splitFileName url of
                (dir, "index.html") -> takeDirectory dir
                _                   -> url


--------------------------------------------------------------------------------
getTeaserContents :: [Item String] -> [Item String]
getTeaserContents = 
  map $ \x -> itemSetBody (fromMaybe (itemBody x) $ 
    needlePrefix "<!--more-->" $ itemBody x) x


--------------------------------------------------------------------------------
transformTextExcept :: (TS.Tag String -> Bool) -- predicate on @TS.TagOpen@
                    -> (String -> String) -- manipulate text if predicate false
                    -> String -> String
transformTextExcept pred f = TS.renderTags . go . TS.parseTags where
    go [] = []
    go (TS.TagText t:xs) = TS.TagText (f t) : go xs
    go (x@(TS.TagOpen tag _):xs)
        | pred x    = x : skip tag 0 xs
        | otherwise = x : go xs
    go (x:xs) = x : go xs
    skip tag l (x@(TS.TagOpen _ _):xs)
        | x TS.~== TS.TagOpen tag [] = x : skip tag (succ l) xs
        | otherwise                  = x : skip tag l xs
    skip tag l (x@(TS.TagClose _):xs)
        | x TS.~== TS.TagClose tag = case l of
                                         0 -> x : go xs
                                         _ -> x : skip tag (l-1) xs
        | otherwise                = x : skip tag l xs
    skip tag l (x:xs)              = x : skip tag l xs


--------------------------------------------------------------------------------
hyphenateItem :: Item String -> Compiler (Item String)
hyphenateItem item = return
                   $ flip itemSetBody item
                   $ transformTextExcept codeOrMaths hy (itemBody item)
  where
    codeOrMaths :: TS.Tag String -> Bool
    codeOrMaths x@(TS.TagOpen name attrs)
        | x TS.~== TS.TagOpen ("code" :: String) [] = True
        | ("class", "math display") `elem` attrs    = True
        | ("class", "math inline") `elem` attrs     = True
        | otherwise                                 = False
    codeOrMaths _ = False
    hy = concat
       . map (intercalate hyphen . hyphenate english_US)
       . (split . condense . whenElt) (\c -> isSpace c || c == '-')
    hyphen = "\x00ad"
