module SyntaxCompletion (computeCand, computeCandHaskell) where

import Lexer(Token(..))

import HaskellLexer
import HaskellParser
import HaskellAST
import HaskellToken
import Terminal

import CommonParserUtil 

import System.IO

-- for syntax completion
import SynCompInterface
import Control.Exception

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

-- | computeCand
computeCand debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode =
  computeCandHaskell debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode Nothing
                                                                                                  
computeCandHaskell :: Bool -> Int -> String -> String -> Bool -> Maybe Token -> IO [EmacsDataItem]
computeCandHaskell debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode haskellOption = (do
  {- 1. Lexing  -}                                                                         
  (line, column, terminalListUptoCursor)  <-
    mainHaskellLexerWithLineColumn 1 1 programTextUptoCursor

  {- 2. Parsing -}
  ((do ast <- runAutomatonHaskell debug 0
                      haskell_actionTable haskell_gotoTable haskell_prodRules
                      pFunList terminalListUptoCursor
                      haskellOption
       successfullyParsed)

    `catch` \parseError ->
      case parseError :: ParseError Token DummyAST of
        _ ->
          {- 3. Lexing the rest and computing candidates with it -}
          do (_, _, terminalListAfterCursor) <-
               mainHaskellLexerWithLineColumn line column programTextAfterCursor
             handleParseError debug maxLevel isSimpleMode terminalListAfterCursor parseError))

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
