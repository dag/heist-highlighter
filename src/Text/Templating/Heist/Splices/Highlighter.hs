module Text.Templating.Heist.Splices.Highlighter (highlighterSplice) where

import Data.ByteString                  (ByteString)
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
      lexer    = fromMaybe defaultLexer $
                 maybe (Just defaultLexer) (lexerByAlias . T.unpack) language
    case runLexer lexer $ nodeBytes node of
      Right tokens -> return . renderHtmlNodes $ format linenos tokens
      Left _       -> textSplice . X.nodeText $ node

nodeBytes :: X.Node -> ByteString
nodeBytes =
    encodeUtf8 . (`T.snoc` '\n') . T.dropAround (== '\n') . X.nodeText

lexerByAlias :: String -> Maybe Lexer
lexerByAlias a =
    listToMaybe . filter (elem a . lAliases) . map snd $ lexers
