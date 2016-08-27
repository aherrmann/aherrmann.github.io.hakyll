{-# LANGUAGE OverloadedStrings #-}


import           Clay
import qualified Clay.Media as M
import           Clay.Stylesheet (key)
import           Data.Monoid
import qualified Data.Text.Lazy.IO as T
import           Prelude hiding ((**), div, rem, span)


main :: IO ()
-- main = T.putStr $ renderWith pretty [] mainCss
main = T.putStr $ renderWith compact [] mainCss


mainCss :: Css
mainCss = do
  globalSettings
  fontSettings
  layoutSettings
  mathsSettings


globalSettings = do
  star ? do
    boxSizing borderBox


fontSettings = do
  body ? do
    fontFamily ["Merriweather Sans"] [sansSerif]
    textRendering optimizeLegibility
  header ** p ? do
    fontFamily ["Merriweather Sans"] [sansSerif]
    fontWeight (weight 400)
    fontStyle italic
    color "#666666"
  headings ? do
    fontFamily ["Merriweather"] [serif]
    fontWeight (weight 900)
    color "#333333"
    a ? do
      color "#333333"
  "#page-title" |> h1 |> a ? do
    fontFamily ["Merriweather"] [serif]
    fontWeight (weight 700)
    fontStyle italic
    query M.screen [M.minWidth (px 440)] $ do
      fontSize (em 1.5)
    fontSize (vw 9)
    color "#333333"
  "#page-footer" ? do
    color "#999999"
    a ? do
      color "#444444"
    a # visited ? do
      color "#666666"
  "#section-title" ? do
    fontFamily ["Merriweather"] [serif]
    fontWeight (weight 700)
    fontStyle italic
    fontSize (em 1.5)
    color "#999999"
  "#post-header" |> h1 ? do
    query M.screen [M.minWidth (px 380)] $ do
      fontSize (em 2.5)
    fontSize (vw 9)
  nav ? do
    fontFamily ["Merriweather"] [sansSerif]
    fontWeight (weight 300)
    fontSize (em 1.5)
    a <> a # visited ? do
      color "#666666"
    a # hover ? do
      textDecoration none
      outline solid (px 2) "#666666"
    a # "disabled" ? do
      color "#999999"
    a # "disabled" # hover ? do
      textDecoration none
      outline none (px 0) transparent
  code <> pre ? do
    fontFamily ["Hasklig"] [monospace]
  pre ? do
    overflowX auto
    overflowY hidden
  a ? do
    textDecoration none
    color "#666666"
  a # hover ? do
    textDecoration underline
  a # visited ? do
    color "#444444"


layoutSettings = do
  body ? do
    let minMargin = 3
    sym2 margin (px 0) (px minMargin)
    query M.screen [M.minWidth (px (800 + 2*minMargin))] $ do
      maxWidth (px 800)
      sym2 margin (px 0) auto
  main_ ? do
    maxWidth (px 650)
    sym2 margin (px 10) auto
  "#page-header" ? do
    sym2 margin (px 0) (px 5)
    marginBottom (px 10)
  nav |> ul ? do
    listStyleType none
    sym padding (px 0)
    sym2 margin (px 0) auto
    textAlign center
    li ? do
      display inline
      a ? do
        display inlineBlock
        sym padding (px 10)
    -- li |+ li # before ? do
    --   return ()
  ".menu-text" <> ".post-nav-text" ? do
    display none
  query M.screen [M.minWidth (px 550)] $
    ".menu-text" <> ".post-nav-text" ? do
      display inherit
  "#post-nav" ? do
    marginTop (px 20)
    marginBottom (px 10)
  "#page-footer" ? do
    p ? do
      textAlign center
  ".post-preview" ? do
    marginBottom (em 3)
  "#post-content" ? do
    marginBottom (px 30)
  hr ? do
    border double (px 0) black
    height (px 1)
    backgroundImage $ linearGradient
      (straight sideRight)
      [ (transparent, pct 0)
      , (black, pct 50)
      , (transparent, pct 100) ]
  p ? do
    textAlign justify
  pre ? do
    sym2 margin (em 0) (em 1)


mathsSettings = do
    ".katex-display" ? do
        overflowX scroll


headings = h1 <> h2 <> h3 <> h4 <> h5 <> h6
