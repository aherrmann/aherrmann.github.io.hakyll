module Highlight (
  syntaxHighlight
) where


import           Control.Monad             ((<=<))
import           Control.Monad.Error.Class (throwError)
import           Hakyll
import           Text.Pandoc               (Block (..), Format (..),
                                            Inline (..), Pandoc (..))
import           Text.Pandoc.Walk          (walkM)


syntaxHighlight :: Pandoc -> Compiler Pandoc
syntaxHighlight = walkM highlightBlock <=< walkM highlightInline


-- | Syntax highlighting on code blocks
--
-- Code is highlighted using pygmentize and wrapped
-- in a div tag with class 'highlight' and a pre tag underneath.
highlightBlock :: Block -> Compiler Block

-- no need to send plain text through pygmentize
highlightBlock x@(CodeBlock (_, [], _) _) = pure x

highlightBlock (CodeBlock (_, [lang], opts) str) = do
  toHtml <$> pygmentize lang opts str
  where
    toHtml = RawBlock (Format "html")

highlightBlock (CodeBlock (cls, langs, opts) _) =
  throwError
    [ "Highlight error"
    , "Invalid language"
    , show cls, show langs, show opts ]

highlightBlock x = pure x


-- | Syntax highlighting on inline code
--
-- Code is highlighted using pygmentize and wrapped
-- in a code tag with class 'highlight-inline'.
highlightInline :: Inline -> Compiler Inline

-- no need to send plain text through pygmentize
highlightInline x@(Code (_, [], _) _) = pure x

highlightInline (Code (_, [lang], opts) str) = do
  -- inline code should not contain new-lines
  let str' = replaceNewlines str
      opts' = ("nowrap", "True"):("lineseparator",""):opts
  toInlineHtml <$> pygmentize lang opts' str'
  where
    replaceNewlines = map (\x -> if x == '\n' then ' ' else x)
    toInlineHtml content =
      RawInline
        (Format "html")
        ("<code class=\"highlight-inline\">" ++ content ++ "</code>")

highlightInline (Code (cls, langs, opts) _) =
  throwError
    [ "Highlight error"
    , "Invalid language"
    , show cls, show langs, show opts ]

highlightInline x = pure x


type Lexer = String


type Options = [(String, String)]


pygmentize :: Lexer -> Options -> String -> Compiler String
pygmentize lexer opts =
  unixFilter
    "pygmentize"
    ("-l":lexer:"-f":"html":concatMap toPOpt opts)
  where
    toPOpt (opt, value) = ["-P", opt ++ "=" ++ value]
