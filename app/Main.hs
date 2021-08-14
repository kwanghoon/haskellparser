module Main where

-- [NOTE]
-- To use modules in ghc-parser-lib package,
-- refer to https://www.stackage.org/nightly-2019-09-04/package/ghc-lib-parser-8.8.0.20190424
--
-- For example, import Lexer, not import GHC.Parser.Lexer. 

import HaskellLexer


{-
[Running]

Given

 do { x <- m; return x }

this haskell lexer produces a list of
tokens and line and column information.

$ stack exec lexer-exe
(1,1,1,3): ITdo
(1,4,1,5): ITocurly
(1,6,1,7): ITvarid "x"
(1,8,1,10): ITlarrow NormalSyntax
(1,11,1,12): ITvarid "m"
(1,12,1,13): ITsemi
(1,14,1,20): ITvarid "return"
(1,21,1,22): ITvarid "x"
(1,23,1,24): ITccurly
-}

--
main :: IO ()
main = do
  tokenInfos <- mainHaskellLexer "do { x <- m; return x }"
  case tokenInfos of
    [] -> putStrLn "failed..."
    _  -> prTokInfos tokenInfos

