module SyntaxCompletionSpec where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import System.IO (readFile)

spec debug maxLevel = hspec $ do
  describe "syntax complection hslexer/app/syntaxcompletion" $ do
    let hscode = "module Main where\n\n main = do "
    let hscode_after = ""
    it ("[simple] " ++ hscode) $ do
      results <- computeCand debug maxLevel hscode hscode_after True
      results `shouldBe` []

    it ("[nested] " ++ hscode) $ do
      results <- computeCand debug maxLevel hscode hscode_after False
      results `shouldBe` []

