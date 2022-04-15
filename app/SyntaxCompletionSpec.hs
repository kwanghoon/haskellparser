module SyntaxCompletionSpec where

import Lexer(Token(..))
import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec
import HaskellParserUtil

import System.IO (readFile)

spec debug args =
  do
    nameHscodes <- mapM (\arg -> do text <- readFile arg; return (arg,text)) args
    let hscode_after = ""

    hspec $ do
      describe "syntax complection hslexer/app/syntaxcompletion" $ do

        mapM_ (\(name,hscode) -> do
          it ("[simple] " ++ name) $ do
            results <- computeCand debug hscode hscode_after True
            results `shouldBe` []

          it ("[nested] " ++ name) $ do
            results <- computeCand debug hscode hscode_after False
            results `shouldBe` []

              ) nameHscodes
