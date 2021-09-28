module SyntaxCompletionSpec where

import Lexer(Token(..))
import SyntaxCompletion (computeCandHaskell)
import SynCompInterface
import Test.Hspec

import System.IO (readFile)

spec debug maxLevel args = do
  nameHscodes <- mapM (\arg -> do text <- readFile arg; return (arg,text)) args
  let hscode_after = ""
  
  hspec $ do
    describe "syntax complection hslexer/app/syntaxcompletion" $ do

      mapM_ (\(name,hscode) -> 
        it ("[simple] " ++ name) $ do
          results <- computeCandHaskell debug maxLevel hscode hscode_after True (Just ITvccurly)
          results `shouldBe` []) nameHscodes

      -- it ("[nested] " ++ name) $ do
      --   results <- computeCandHaskell debug maxLevel hscode hscode_after False (Just ITvccurly)
      --   results `shouldBe` []

