{-# LANGUAGE OverloadedStrings #-}
import           Clay
import           Data.Monoid
import qualified Data.Text.Lazy.IO as T
import           Prelude hiding ((**),rem)

main :: IO ()
main = T.putStr $ renderWith pretty [] mainCss
-- main = T.putStr $ renderWith compact [] mainCss

whiteColor :: Color
whiteColor = "#F6F2F1"

lightBlueColor :: Color
lightBlueColor = "#8A96A6"

blueColor :: Color
blueColor = "#44495D"

darkBlueColor :: Color
darkBlueColor = "#242335"

bodyBgColor :: Color
bodyBgColor = whiteColor

titleBgColor :: Color
titleBgColor = darkBlueColor

titleH1Color :: Color
titleH1Color = whiteColor

titleH2Color :: Color
titleH2Color = lightBlueColor

menuItemColor :: Color
menuItemColor = lightBlueColor

menuItemBgColor :: Color
menuItemBgColor = blueColor

footerColor :: Color
footerColor = lightBlueColor

mainCss :: Css
mainCss = do
    (h1 <> h2 <> h3 <> h4 <> h5 <> h6) ? a ?
        textDecoration none
    body ? do
        maxWidth (px 700)
        margin (px 0) auto (px 0) auto
        backgroundColor bodyBgColor
        section <? do
            marginLeft (px 20)
            marginRight (px 20)
    header # "#page-header" ? do
        backgroundColor titleBgColor
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
            color titleH1Color
            fontFamily ["verdana"] [sansSerif]
            fontSize (rem 3.0)
        (h2 <> (h2 ** star)) <? do
            color titleH2Color
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
            backgroundColor menuItemBgColor
            a <? do
                color menuItemColor
                display block
                textAlign $ alignSide sideCenter
                textDecoration none
                padding (em 1) (em 1) (em 1) (em 1)
        li # hover <? do
            backgroundColor whiteColor
        li # firstChild <? do
            borderRadius (px 20) (px 0) (px 0) (px 0)
        li # lastChild <? do
            borderRadius (px 0) (px 20) (px 0) (px 0)
    footer # "#page-footer" ? do
        textAlign $ alignSide sideCenter
        color footerColor
        a # link ? color blueColor
        a # visited ? color lightBlueColor
