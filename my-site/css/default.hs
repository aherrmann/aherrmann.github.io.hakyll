{-# LANGUAGE OverloadedStrings #-}
import           Clay
import           Clay.Stylesheet (key)
import           Data.Monoid
import qualified Data.Text.Lazy.IO as T
import           Prelude hiding ((**),rem)

main :: IO ()
main = T.putStr $ renderWith pretty [] mainCss
-- main = T.putStr $ renderWith compact [] mainCss


--------------------------------------------------------------------------------
headings :: Selector
headings = h1 <> h2 <> h3 <> h4 <> h5 <> h6

headings_ :: Selector
headings_ = headings <> headings ** star

a_ :: Selector
a_ = a <> a ** star

avisited_ :: Selector
avisited_ = a # visited <> a # visited ** star

ahover_ :: Selector
ahover_ = a # hover <> a # hover ** star


--------------------------------------------------------------------------------
textStroke :: Color -> Css
textStroke c = do
    key "-webkit-text-stroke" $ value (px 3) <> " " <> value c
    key "text-shadow" $ [ shadow x y | (x, y) <- xys ]
  where
    shadow x y = (value $ px x) <> " "
              <> (value $ px y) <> " "
              <> (value $ px 0) <> " "
              <> (value $ c)
    xys = [ ((-1),    1), (0,    2), (1,    1)
          , ((-2),    0),            (2,    0)
          , ((-1), (-1)), (0, (-2)), (1, (-1)) ]

fontSmooth :: Css
fontSmooth = do
    "-webkit-font-smoothing" -: "subpixel-antialiased" -- "antialiased"
    "-moz-osx-font-smoothing" -: "grayscale"


--------------------------------------------------------------------------------
color1 :: Color
color1 = "#354458"

color2 :: Color
color2 = "#3A9AD9"

color3 :: Color
color3 = "#29ABA4"

color4 :: Color
color4 = "#E9E0D6"

color5 :: Color
color5 = "#EB7260"


--------------------------------------------------------------------------------
pageBackgroundColor :: Color
pageBackgroundColor = color4

headerBackgroundColor :: Color
headerBackgroundColor = color1

headerBackground :: Css
headerBackground = do
    backgroundColor headerBackgroundColor
    background $
        vGradient (headerBackgroundColor -. 10) headerBackgroundColor

headerTitleColor :: Color
headerTitleColor = color4

headerTitleBorderColor :: Color
headerTitleBorderColor = color5

headerSubtitleColor :: Color
headerSubtitleColor = color2

menuBackgroundColor :: Color
menuBackgroundColor = color4 -. 20

menuBackground :: Css
menuBackground = do
    backgroundColor menuBackgroundColor
    background $
        vGradient (menuBackgroundColor -. 10) menuBackgroundColor

menuItemColor :: Color
menuItemColor = color1 -. 30

menuHoverBackgroundColor :: Color
menuHoverBackgroundColor = color3

menuHoverBackground :: Css
menuHoverBackground = do
    backgroundColor menuHoverBackgroundColor
    background $
        vGradient (menuHoverBackgroundColor -. 10) menuHoverBackgroundColor

menuItemHoverColor :: Color
menuItemHoverColor = color1 -. 20

sectionBackgroundColor :: Color
sectionBackgroundColor = pageBackgroundColor -. 10

sectionBackground :: Css
sectionBackground = do
    backgroundColor sectionBackgroundColor
    background $
        vGradient (sectionBackgroundColor -. 10) sectionBackgroundColor

sectionTitleColor :: Color
sectionTitleColor = pageBackgroundColor -. 50

footerColor :: Color
footerColor = color4 -. 70

footerLinkColor :: Color
footerLinkColor = footerColor +. 20

footerVisitedColor :: Color
footerVisitedColor = footerColor

footerHoverColor :: Color
footerHoverColor = footerColor -. 50

headingColor :: Color
headingColor = color1 -. 30

linkColor :: Color
linkColor = color2 -. 50

linkVisitedColor :: Color
linkVisitedColor = color1

linkHoverColor :: Color
linkHoverColor = color3 -. 30

textColor :: Color
textColor = black +. 50


--------------------------------------------------------------------------------
fontSettings :: Css
fontSettings = do
    headings_ ? do
        color $ headingColor
        fontFamily ["Merriweather Sans"] [sansSerif]
        textDecoration none
    nav ** star ? do
        fontFamily ["Merriweather Sans"] [sansSerif]
        fontWeight bold
    a_ ? do
        color linkColor
        textDecoration none
    avisited_ ? do
        color linkVisitedColor
    ahover_ ? do
        color linkHoverColor
    code <> code ** star ? do
        fontFamily ["Droid Sans Mono"] []
    star ? do
        fontFamily ["Merriweather", "Georgia"] [serif]
        color textColor


--------------------------------------------------------------------------------
mainCss :: Css
mainCss = do
    fontSettings
    pageHeaderCss
    pageFooterCss
    body ? do
        maxWidth (px 700)
        margin (px 0) auto (px 0) auto
        backgroundColor pageBackgroundColor
        section <? do
            marginLeft (px 10)
            marginRight (px 10)
    h1 # ".section-title" ? do
        borderBottom solid (px 1) sectionTitleColor
        color sectionTitleColor
        fontSize (rem 1.6)
        fontStyle italic
        marginLeft (px 20)
        marginRight (px 20)
        paddingTop (rem 1)
    article # ".post-preview" <> article # "#post" ? do
        sectionBackground
        borderRadius (px 10) (px 10) (px 10) (px 10)
        margin (px 10) (px 0) (px 10) (px 0)
        padding (px 20) (px 20) (px 20) (px 20)
        textAlign justify
    (header # ".post-preview-header" <> header # "#post-header") ** h1 ? do
        margin (px 0) (px 0) (px 0) (px 0)
    section # "#sharing" ? do
        float floatLeft
        display inlineBlock
        margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    nav # "#post-nav" |> ul ? do
        display block
        fontSize (px 0)
        listStyleType none
        margin (px 0) (px 0) (px 0) (px 0)
        overflow hidden
        padding (px 0) (px 0) (px 0) (px 0)
        textAlign $ alignSide sideRight
        li <? do
            fontSize (rem 1.2)
            display inlineBlock
            menuBackground
            borderRadius (em 0.5) (em 0.5) (em 0.5) (em 0.5)
            margin (rem 1) (rem 0) (rem 1) (rem 1)
            a <? do
                color menuItemColor
                display block
                textAlign $ alignSide sideCenter
                textDecoration none
                padding (em 0.5) (em 0.5) (em 0.5) (em 0.5)
        li # hover <? do
            menuHoverBackground
            a <? do
                color menuItemHoverColor


--------------------------------------------------------------------------------
pageHeaderCss :: Css
pageHeaderCss = do
    header # "#page-header" ? do
        headerBackground
        borderRadius (px 0) (px 0) (px 20) (px 20)
        marginTop (px 0)
        padding (px 20) (px 20) (px 0) (px 20)
        textAlign $ alignSide sideCenter
    header # "#page-title" ? do
        display inlineBlock
        width auto
        height auto
        margin (px 0) auto (px 0) auto
        textAlign $ alignSide sideRight
        (h1 <> (h1 ** star)) <? do
            color headerTitleColor
            textStroke headerTitleBorderColor
            fontSmooth
            fontSize (rem 4.0)
        (h2 <> (h2 ** star)) <? do
            color headerSubtitleColor
    nav # "#page-nav" ? do
        display block
        margin (px 0) auto (px 0) auto
    nav # "#page-nav" |> ul ? do
        listStyleType none
        margin (px 0) (px 0) (px 0) (px 0)
        padding (px 0) (px 0) (px 0) (px 0)
        overflow hidden
        fontSize (px 0)
        li <? do
            fontSize (rem 1.2)
            display inlineBlock
            menuBackground
            a <? do
                color menuItemColor
                display block
                textAlign $ alignSide sideCenter
                textDecoration none
                padding (em 1) (em 1) (em 1) (em 1)
        li # hover <? do
            menuHoverBackground
        li # firstChild <? do
            borderRadius (px 20) (px 0) (px 0) (px 0)
        li # lastChild <? do
            borderRadius (px 0) (px 20) (px 0) (px 0)


--------------------------------------------------------------------------------
pageFooterCss :: Css
pageFooterCss = do
    footer # "#page-footer" ? do
        textAlign $ alignSide sideCenter
        marginTop (rem 3)
        star ? color footerColor
        a ? color footerLinkColor
        a # visited ? color footerVisitedColor
        a # hover ? color footerHoverColor
