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


--------------------------------------------------------------------------------
headings :: Selector
headings = h1 <> h2 <> h3 <> h4 <> h5 <> h6


--------------------------------------------------------------------------------
bevel :: Css
bevel = "box-shadow" -: "0 1px 2px #fff,\n\
                        \0 -1px 1px #666,\n\
                        \inset 0 -1px 1px rgba(0,0,0,0.5),\n\
                        \inset 0 1px 1px rgba(255,255,255,0.8),\n\
                        \1px 0 2px #fff,\n\
                        \-1px 0 1px #666,\n\
                        \inset -1px 0 1px rgba(0,0,0,0.5),\n\
                        \inset 1px 0 1px rgba(255,255,255,0.8);"

fontSmooth :: Css
fontSmooth = do
    "-webkit-font-smoothing" -: "subpixel-antialiased" -- "antialiased"
    "-moz-osx-font-smoothing" -: "grayscale"

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

postDateColor :: Color
postDateColor = color4 -. 130

codeBackgroundColor :: Color
codeBackgroundColor = pageBackgroundColor

inlineCodeColor :: Color
inlineCodeColor = textColor +. 30

linkColor :: Color
linkColor = color2 -. 50

linkVisitedColor :: Color
linkVisitedColor = color1 +. 30

linkHoverColor :: Color
linkHoverColor = color3 -. 30

textColor :: Color
textColor = black +. 50


--------------------------------------------------------------------------------
fontSettings :: Css
fontSettings = do
    headings <> headings |> a ? do
        color $ headingColor
        fontFamily ["Merriweather Sans"] [sansSerif]
        textDecoration none
    nav ** (a <> span) ? do
        fontFamily ["Merriweather Sans"] [sansSerif]
        fontWeight bold
    a <> a |> span ? do
        color linkColor
        textDecoration none
    a # visited <> a # visited |> span ? do
        color linkVisitedColor
    a # hover <> a # hover |> span ? do
        color linkHoverColor
    code <> code |> span ? do
        fontFamily ["Droid Sans Mono"] []
    star ? do
        fontFamily ["Merriweather", "Georgia"] [serif]
        color textColor


--------------------------------------------------------------------------------
pageHeaderSettings :: Css
pageHeaderSettings = do
    query M.screen [M.maxWidth (px 570)] $
        ".menu-text" ? display none
    "#page-header" ? do
        headerBackground
        borderRadius (rem 0) (rem 0) (rem 4) (rem 4)
        marginTop (rem 0)
        padding (rem 0) (rem 2) (rem 0) (rem 2)
        textAlign $ alignSide sideCenter
    "#page-title" ? do
        display inlineBlock
        width auto
        height auto
        margin (rem 0) auto (rem 0) auto
        textAlign $ alignSide sideRight
        h1 |> a <? do
            color headerTitleColor
            fontSmooth
            fontSize (rem 4.0)
            query M.screen [M.maxWidth (px 612)] $
                fontSize (rem 3.0)
            query M.screen [M.maxWidth (px 480)] $
                fontSize (rem 2.2)
            query M.screen [M.minWidth (px 481)] $
                textStroke headerTitleBorderColor
            margin (rem 3) (rem 0) (rem 1) (rem 0)
        h2 <? do
            color headerSubtitleColor
            margin (rem 0) (rem 0) (rem 2) (rem 0)
    "#page-nav" ? do
        display block
        margin (rem 0) auto (rem 0) auto
    "#page-nav" |> ul ? do
        listStyleType none
        margin (rem 0) (rem 0) (rem 0) (rem 0)
        padding (rem 0) (rem 0) (rem 0) (rem 0)
        overflow hidden
        fontSize (rem 0)
        li <? do
            fontSize (rem 1.2)
            display inlineBlock
            a <? do
                menuBackground
                color menuItemColor
                display block
                textAlign $ alignSide sideCenter
                textDecoration none
                padding (rem 1.2) (rem 1.2) (rem 1.2) (rem 1.2)
            a <> (a |> span) <? do
                color menuItemColor
            a # hover <? do
                menuHoverBackground
        li # firstChild |> a ? do
            borderRadius (rem 2) (rem 0) (rem 0) (rem 0)
        li # lastChild |> a ? do
            borderRadius (rem 0) (rem 2) (rem 0) (rem 0)


--------------------------------------------------------------------------------
pageFooterSettings :: Css
pageFooterSettings = do
    footer # "#page-footer" ? do
        marginTop (rem 3)
        textAlign $ alignSide sideCenter
        p <? do
            color footerColor
            a <? color footerLinkColor
            a # visited <? color footerVisitedColor
            a # hover <? color footerHoverColor


--------------------------------------------------------------------------------
postSettings :: Css
postSettings = do
    ".post-preview" <> "#post" ? do
        sectionBackground
        borderRadius (rem 1.5) (rem 1.5) (rem 1.5) (rem 1.5)
        margin (rem 1) (rem 0) (rem 1) (rem 0)
        padding (rem 1.3) (rem 1.3) (rem 1.3) (rem 1.3)
        query M.screen [M.maxWidth (px 570)] $ do
            padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    (".post-preview-header" <> "#post-header") |> h1 ? do
        margin (rem 0) (rem 0) (rem 0) (rem 0)
        textAlign $ alignSide sideCenter
    (".post-preview-header" <> "#post-header") |> p ? do
        color postDateColor
        fontStyle italic
        textAlign $ alignSide sideRight
    "#sharing" ? do
        float floatLeft
        display inlineBlock
        margin (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
    "#post-nav" |> ul ? do
        display block
        fontSize (rem 0)
        listStyleType none
        margin (rem 0) (rem 0) (rem 0) (rem 0)
        overflow hidden
        padding (rem 0) (rem 0) (rem 0) (rem 0)
        textAlign $ alignSide sideRight
        li <? do
            display inlineBlock
            margin (rem 1) (rem 0) (rem 1) (rem 1)
            a <? do
                menuBackground
                borderRadius (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
                color menuItemColor
                display block
                fontSize (rem 1.2)
                textAlign $ alignSide sideCenter
                textDecoration none
                padding (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
            a # hover <? do
                menuHoverBackground
                color menuItemHoverColor


--------------------------------------------------------------------------------
codeSettings :: Css
codeSettings = do
    pre ? do
        backgroundColor codeBackgroundColor
        bevel
        borderRadius (rem 0.5) (rem 0.5) (rem 0.5) (rem 0.5)
        padding (rem 0.5) (rem 0.5) (rem 0.7) (rem 0.5)
    p |> (code <> a |> code) ? do
        borderRadius (rem 0.3) (rem 0.3) (rem 0.3) (rem 0.3)
        color inlineCodeColor
        padding (rem 0) (rem 0.2) (rem 0) (rem 0.2)


--------------------------------------------------------------------------------
mainCss :: Css
mainCss = do
    fontSettings
    pageHeaderSettings
    pageFooterSettings
    postSettings
    codeSettings
    body ? do
        maxWidth (px 700)
        margin (rem 0) auto (rem 0) auto
        backgroundColor pageBackgroundColor
        query M.screen [M.minWidth (px 571)] $
            section <? do
                marginLeft (rem 0.7)
                marginRight (rem 0.7)
    ".section-title" ? do
        borderBottom solid (px 1) sectionTitleColor
        color sectionTitleColor
        fontSize (rem 1.6)
        fontStyle italic
        marginLeft (rem 1.5)
        marginRight (rem 1.5)
        paddingTop (rem 1)
