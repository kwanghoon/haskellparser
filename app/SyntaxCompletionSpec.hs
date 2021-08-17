module SyntaxCompletionSpec where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import System.IO (readFile)

spec = hspec $ do
  describe "syntax complection yapb/app/syntaxcompletion" $ do
    let hscode = "module Main where\n\n main = "
    it ("[simple] " ++ hscode) $ do
      results <- computeCand False hscode "" True
      results `shouldBe` []

    it ("[nested] " ++ hscode) $ do
      results <- computeCand False hscode "" False
      results `shouldBe` []

