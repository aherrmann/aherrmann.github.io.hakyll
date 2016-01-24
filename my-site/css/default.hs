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
        padding (px 20) (px 20) (px 20) (px 20)
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
