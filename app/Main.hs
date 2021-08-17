module Main where

-- [NOTE]
-- To use modules in ghc-parser-lib package,
-- refer to https://www.stackage.org/nightly-2019-09-04/package/ghc-lib-parser-8.8.0.20190424
--
-- For example, import Lexer, not import GHC.Parser.Lexer. 

-- | Lexing & Parsing
import HaskellLexer
import HaskellParser
import HaskellAST
import Terminal

import CommonParserUtil (runAutomaton)

import System.IO
import System.Environment (getArgs, withArgs)

-- | syntax completion
import EmacsServer

import SyntaxCompletion (computeCand)
import SyntaxCompletionSpec (spec)



--
main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args
  then withArgs [] spec
  else if "emacs" `elem` args
  then emacsServer (computeCand False)
  else _main args

_main [] = return ()
_main (fileName:args) = do
  putStrLn $ "Lexing&Parsing: " ++ fileName
  
  text <- readFile fileName
  
  terminalList <- mainHaskellLexer text
  
  case terminalList of
    [] -> putStrLn "failed..."
    _  -> do mapM_ (\terminal -> putStrLn $ terminalToString terminal) terminalList
             ast <- runAutomaton False 0
                      haskell_actionTable haskell_gotoTable haskell_prodRules
                      pFunList terminalList
             putStrLn $ "Done: " ++ show ast


