{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blaze.ByteString.Builder
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit
import Text.Highlighter.Lexers.Haskell
import Text.Templating.Heist
import Text.Templating.Heist.Splices.Highlighter

import qualified Data.ByteString as B

main :: IO ()
main = $defaultMainGenerator

assertTemplateOutput :: B.ByteString -> String -> Assertion
assertTemplateOutput template fixture =
  do
    ets <- loadTemplates "test/templates" $
           bindSplice "highlight" (highlighterSplice lexer) $
           defaultHeistState
    let ts = either error id ets
    Just (b,_) <- renderTemplate ts template
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
