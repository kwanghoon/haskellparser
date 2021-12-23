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
import HaskellParserUtil

import Terminal
import SyntaxCompletion (computeCandHaskell, CC_HaskellOption(..))

import CommonParserUtil (runAutomatonHaskell, AutomatonSpec(..))

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
  else if null args /= True && "candidate" == head args
  then do maxLevel <- getMaxLevel
          debug <- getDebugOption
          withArgs [] $ _mainGenCand False debug maxLevel (tail args)
  else if null args /= True && "candidateinfo" == head args
  then do maxLevel <- getMaxLevel
          debug <- getDebugOption
          withArgs [] $ _mainGenCand True debug maxLevel (tail args)
  else if null args /= True && "emacs" == head args
  then do maxLevel <- getMaxLevel
          emacsServer
            (\ptuc ptac ism -> computeCandHaskell False maxLevel ptuc ptac ism
                                 (CC_HaskellOption {
                                    vccurly_token=Just ITvccurly,
                                    nonterminalFormatFun=Just haskell_convFun}))
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
  terminalList <- haskellLexer text
  
  case terminalList of
    [] -> putStrLn "failed..."
    _  -> do putStrLn $ "Parsing: "
             debugOpt <- getDebugOption
             mapM_ (\terminal -> putStrLn $ terminalToString terminal) terminalList
             putStrLn ""
             ast <- runAutomatonHaskell debugOpt (AutomatonSpec { am_initState=0,
                      am_actionTbl=haskell_actionTable, am_gotoTbl=haskell_gotoTable, am_prodRules=haskell_prodRules,
                      am_parseFuns=pFunList}) terminalList
                      (Just ITvccurly) -- Haskell option
             putStrLn ""
             putStrLn $ "Done: " ++ show ast

-- --
-- init :: Line -> Column -> String -> (Bool -> P (Terminal Token))
-- init line col str = \b -> unP (next b) (initParseState line col str)

-- next :: Bool -> P (Terminal Token)
-- next True = do
--   popContext
--   next False

-- next False = do 
--   token <- singleHaskellToken
--   case toTerminalToken token of
--     Left terminalToken -> return terminalToken
--     Right (line, col) -> error "Not implemented: nextTerminal"

-- _mainRevised [] = return ()
-- _mainRevised (fileName:args) = do
--   putStrLn $ "Reading: " ++ fileName
  
--   text <- readFile fileName
  
--   putStrLn $ "Lexing & Parsing: "
--   terminalList <- haskellLexer text
  
--   debugOpt <- getDebugOption

--   ast <- runAutomatonHaskell debugOpt (AutomatonSpec { am_initState=0,
--            am_actionTbl=haskell_actionTable, am_gotoTbl=haskell_gotoTable, am_prodRules=haskell_prodRules,
--            am_parseFuns=pFunList}) terminalList
--            (Just ITvccurly) -- Haskell option
--   putStrLn ""
--   putStrLn $ "Done: " ++ show ast

--
_mainGenCand withInfo debug maxLevel [] = return ()
_mainGenCand withInfo debug maxLevel (fileName:args) = do
  putStrLn "Haskell code:"
  hscode_before <- readFile fileName
  let hscode_after = ""
  putStrLn "[before]"
  putStrLn hscode_before
  putStrLn "[after]"
  putStrLn hscode_after
  
  putStrLn "Computing...:"
  results <- computeCandHaskell debug maxLevel hscode_before hscode_after True
                        (CC_HaskellOption {
                          vccurly_token=Just ITvccurly,
                          nonterminalFormatFun=if withInfo then Just haskell_convFun else Nothing})
  putStrLn "Candidates:"
  mapM_ putStrLn $ map show results
  _mainGenCand withInfo debug maxLevel args
