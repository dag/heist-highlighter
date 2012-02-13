module Text.Templating.Heist.Splices.Highlighter (highlighterSplice) where

import Data.Maybe                       (fromMaybe, listToMaybe)
import Data.Text.Encoding               (encodeUtf8)
import Text.Blaze.Renderer.XmlHtml      (renderHtmlNodes)
import Text.Highlighter                 (Lexer, runLexer, lAliases, lexers)
import Text.Highlighter.Formatters.Html (format)
import Text.Templating.Heist            (Splice, getParamNode, textSplice)

import qualified Data.Text    as T
import qualified Text.XmlHtml as X

highlighterSplice :: Monad m => Lexer -> Splice m
highlighterSplice defaultLexer =
  do
    node <- getParamNode
    let
      language = X.getAttribute (T.pack "language") node
      linenos  = X.hasAttribute (T.pack "linenos") node
      bytes    = encodeUtf8 . X.nodeText $ node
      lexer    = fromMaybe defaultLexer $
                 maybe (Just defaultLexer) (lexerByAlias . T.unpack) language
    case runLexer lexer bytes of
      Right tokens -> return . renderHtmlNodes $ format linenos tokens
      Left _       -> textSplice . X.nodeText $ node

lexerByAlias :: String -> Maybe Lexer
lexerByAlias a =
    listToMaybe . filter (elem a . lAliases) . map snd $ lexers
