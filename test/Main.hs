{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder                  (toByteString)
import Test.Framework.Providers.HUnit            (testCase)
import Test.Framework.TH                         (defaultMainGenerator)
import Test.HUnit                                (Assertion, (@?=))
import Text.Highlighter.Lexers.Haskell           (lexer)
import Text.Templating.Heist.Splices.Highlighter (highlighterSplice)

import qualified Data.ByteString       as B
import qualified Text.Templating.Heist as H

main :: IO ()
main = $defaultMainGenerator

assertTemplateOutput :: B.ByteString -> String -> Assertion
assertTemplateOutput template fixture =
  do
    ets <- H.loadTemplates "test/templates" $
           H.bindSplice "highlight" (highlighterSplice lexer) $
           H.defaultHeistState
    let ts = either error id ets
    Just (b,_) <- H.renderTemplate ts template
    f <- B.readFile $ "test/fixtures/" ++ fixture ++ ".html"
    toByteString b @?= f

case_default_lexer :: Assertion
case_default_lexer = assertTemplateOutput "default-lexer" "haskell"

case_default_lexer_with_linenos :: Assertion
case_default_lexer_with_linenos =
    assertTemplateOutput "default-lexer-linenos" "haskell-linenos"

case_override_lexer :: Assertion
case_override_lexer = assertTemplateOutput "override-lexer" "python"

case_override_lexer_with_linenos :: Assertion
case_override_lexer_with_linenos =
    assertTemplateOutput "override-lexer-linenos" "python-linenos"

case_literal_html :: Assertion
case_literal_html = assertTemplateOutput "literal-html" "html"
