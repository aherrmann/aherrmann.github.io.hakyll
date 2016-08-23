{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Data.Char             (isSpace)
import           Data.DateTime         (formatDateTime, getCurrentTime)
import           Data.List             (intercalate)
import           Data.List.Split       (condense, split, whenElt)
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           (mappend, (<>))
import qualified Data.Set              as S
import           Hakyll
import           System.FilePath.Posix (splitFileName, takeDirectory)
import qualified Text.HTML.TagSoup     as TS
import           Text.Hyphenation      (english_US, hyphenate)
import qualified Text.Pandoc.Options   as PO

import           Adjacent
import           Highlight


--------------------------------------------------------------------------------
-- Main Program


main :: IO ()
main = do
  currYear <- formatDateTime "%Y" <$> getCurrentTime
  hakyll $ do

    -- Contexts
    prevNextPostFields <- prevNextByDateFields "Post" "posts/*"
    let pageRoot = "http://aherrmann.github.io"
        pageCtx = mconcat
          [ constField "pageRoot" pageRoot
          , constField "currYear" currYear
          , defaultContext ]
        postCtx = mconcat
          [ directoryUrlField "url"
          , dateField "date" "%B %e, %Y"
          , prevNextPostFields
          , pageCtx ]
        teasCtx = mconcat
          [ teaserField "teaser" "content"
          , postCtx ]

    -- Matchers
    match ( "images/*" .||. "fonts/*" .||. "js/*.js" ) $ do
      route   idRoute
      compile copyFileCompiler

    match "css/*.css" $ do
      route   idRoute
      compile compressCssCompiler

    match "css/*.hs" $ do
      route   $ setExtension "css"
      compile $ getResourceString
          >>= withItemBody (unixFilter "cabal" ["exec", "runghc"])

    match (fromList ["about.rst", "contact.markdown"]) $ do
      route   $ setExtension "html"
      compile $ customPandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" pageCtx
          >>= relativizeUrls

    match "posts/*" $ do
      route   postRoute
      compile $ customPandocCompiler
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
        let feedConfig = FeedConfiguration
                { feedTitle       = "Programming Blog"
                , feedDescription = ""
                , feedAuthorName  = "Andreas Herrmann"
                , feedAuthorEmail = "andreash87@gmx.ch"
                , feedRoot        = pageRoot
                }
            feedCtx = teasCtx <> bodyField "description"
        allTeasers <- getTeaserContents <$> loadAllSnapshots "posts/*" "content"
        recent <- take 10 <$> recentFirst allTeasers
        renderAtom feedConfig feedCtx recent

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
-- Pandoc


readerOpts :: PO.ReaderOptions
readerOpts =
  defaultHakyllReaderOptions
    { PO.readerExtensions = foldr S.insert defaultExtensions extensions }
  where
    defaultExtensions = PO.readerExtensions defaultHakyllReaderOptions
    extensions =
      [ PO.Ext_tex_math_dollars
      , PO.Ext_tex_math_double_backslash
      , PO.Ext_latex_macros
      , PO.Ext_inline_code_attributes ]


writerOpts :: PO.WriterOptions
writerOpts =
  defaultHakyllWriterOptions
    { PO.writerHTMLMathMethod = PO.MathJax ""
    , PO.writerHtml5 = True }


customPandocCompiler :: Compiler (Item String)
customPandocCompiler =
  pandocCompilerWithTransformM readerOpts writerOpts syntaxHighlight


--------------------------------------------------------------------------------
-- Routing


-- | Construct route of a blog post
--
-- Posts are routed as `<category>/<yyyy>/<mm>/<dd>/<title>/index.html`.
postRoute :: Routes
postRoute =
    metadataRoute makeCategoryPath `composeRoutes`
    postDateRoute `composeRoutes`
    setExtension "html" `composeRoutes`
    asIndexRoute "html"


makeCategoryPath :: Metadata -> Routes
makeCategoryPath md =
    gsubRoute "posts/" $ const . (++ "/") $
    fromMaybe (error "Posts: Post without category!") $
    lookupString "category" md


postDateRoute :: Routes
postDateRoute =
    gsubRoute "/[0-9]{4}-[0-9]{2}-[0-9]{2}-" $ replaceAll "-" (const "/")


asIndexRoute :: String -> Routes
asIndexRoute extension =
    gsubRoute ('.':extension) $ const ("/index." ++ extension)


----------------------------------------------------------------------
-- Context Fields


-- | Transform a URL field to one that crops the /index.html in the end.
--
-- So that blog articles and other pages are linked to as `<some>/<path>` instead of `<some>/<path>/index.html`.
-- If the url does not end on `index.html` then the original url is given.
directoryUrlField :: String -> Context a
directoryUrlField key = mapContext removeIndexStr (urlField key)
    where
        -- Credit to Yann Esposito, with minor changes
        -- http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
        removeIndexStr :: String -> String
        removeIndexStr url =
            case splitFileName url of
                (dir, "index.html") -> takeDirectory dir
                _                   -> url


----------------------------------------------------------------------
-- Teaser


getTeaserContents :: [Item String] -> [Item String]
getTeaserContents =
  map $ \x -> itemSetBody (fromMaybe (itemBody x) $
    needlePrefix "<!--more-->" $ itemBody x) x


----------------------------------------------------------------------
-- Hyphenation


-- | Transform the text between tags unless the given predicate holds.
--
-- @transformTextExcept p f str@ reads @str@ as html.
-- It applies @f@ to all text elements and replaces them with @f@'s result unless @p@ holds on a parent tag.
--
-- E.g.
-- @
--     transformTextExcept (~== TagOpen "code") fun
-- @
-- would transform all text using @fun@ except text which is contained within a pair of @<code>@ @</code>@ tags.
transformTextExcept :: (TS.Tag String -> Bool) -- ^ predicate on @TS.TagOpen@
                    -> (String -> String) -- ^ manipulate text if predicate false
                    -> String -> String
transformTextExcept p f = TS.renderTags . go . TS.parseTags where
    go [] = []
    go (TS.TagText t:xs) = TS.TagText (f t) : go xs
    go (x@(TS.TagOpen tag _):xs)
        | p x       = x : skip tag 0 xs
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
    skip _ _ [] = []


-- | Hyphenate all text except for code and maths.
hyphenateItem :: Item String -> Compiler (Item String)
hyphenateItem item = return
                   $ flip itemSetBody item
                   $ transformTextExcept codeOrMaths hy (itemBody item)
  where
    codeOrMaths :: TS.Tag String -> Bool
    codeOrMaths x@(TS.TagOpen _ attrs)
        | x TS.~== TS.TagOpen ("code" :: String) [] = True
        | x TS.~== TS.TagOpen ("pre" :: String)  [] = True
        | ("class", "math display") `elem` attrs    = True
        | ("class", "math inline") `elem` attrs     = True
        | otherwise                                 = False
    codeOrMaths _ = False
    hy = concatMap (intercalate hyphen . hyphenate english_US)
       . split splitter
    splitter = condense $ whenElt (\c -> isSpace c || c == '-')
    hyphen = "\x00ad"
