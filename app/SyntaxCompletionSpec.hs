module SyntaxCompletionSpec where

import Lexer(Token(..))
import SyntaxCompletion (computeCandHaskell, CC_HaskellOption(..))
import SynCompInterface
import Test.Hspec
import HaskellParserUtil

import System.IO (readFile)

spec debug maxLevel args =
  do
    nameHscodes <- mapM (\arg -> do text <- readFile arg; return (arg,text)) args
    let hscode_after = ""

    hspec $ do
      describe "syntax complection hslexer/app/syntaxcompletion" $ do

        mapM_ (\(name,hscode) -> do
          it ("[simple] " ++ name) $ do
            results <- computeCandHaskell debug maxLevel hscode hscode_after True
                        (CC_HaskellOption {
                            vccurly_token=Just ITvccurly,
                            nonterminalFormatFun=Just haskell_convFun})
            results `shouldBe` []

          it ("[nested] " ++ name) $ do
            results <- computeCandHaskell debug maxLevel hscode hscode_after False
                        (CC_HaskellOption {
                            vccurly_token=Just ITvccurly,
                            nonterminalFormatFun=Just haskell_convFun})
            results `shouldBe` []

              ) nameHscodes
