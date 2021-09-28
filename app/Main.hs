module Main where

-- [NOTE]
-- To use modules in ghc-parser-lib package,
-- refer to https://www.stackage.org/nightly-2019-09-04/package/ghc-lib-parser-8.8.0.20190424
--
-- For example, import Lexer, not import GHC.Parser.Lexer. 

-- | Lexing & Parsing
import Lexer(Token(..))
import HaskellLexer
import HaskellParser
import HaskellAST
import Terminal

import CommonParserUtil (runAutomatonHaskell)

import System.IO
import System.Environment (getArgs, withArgs)

-- | syntax completion
import EmacsServer

import SyntaxCompletion (computeCandHaskell)
import SyntaxCompletionSpec (spec)



--
main :: IO ()
main = do
  args <- getArgs
  if null args /= True && "test" == head args
  then do maxLevel <- getMaxLevel
          debug <- getDebugOption
          withArgs [] $ spec debug maxLevel (tail args)
  else if null args /= True && "emacs" == head args
  then do maxLevel <- getMaxLevel
          emacsServer
            (\ptuc ptac ism -> computeCandHaskell False maxLevel ptuc ptac ism (Just ITvccurly))
  else _main args

getMaxLevel = do
  putStrLn "Max level for search (e.g., 100): "
  maxLevel_str <- getLine
  return (read maxLevel_str :: Int)

getDebugOption = do
  putStrLn "Debug option (True/False): "
  debug <- getLine
  return (read debug :: Bool)

_main [] = return ()
_main (fileName:args) = do
  putStrLn $ "Reading: " ++ fileName
  
  text <- readFile fileName
  
  putStrLn $ "Lexing: "
  terminalList <- mainHaskellLexer text
  
  case terminalList of
    [] -> putStrLn "failed..."
    _  -> do putStrLn $ "Parsing: "
             debugOpt <- getDebugOption
             mapM_ (\terminal -> putStrLn $ terminalToString terminal) terminalList
             putStrLn ""
             ast <- runAutomatonHaskell debugOpt 0
                      haskell_actionTable haskell_gotoTable haskell_prodRules
                      pFunList terminalList
                      (Just ITvccurly) -- Haskell option
             putStrLn ""
             putStrLn $ "Done: " ++ show ast


