module SyntaxCompletion (computeCand) where

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
computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode = (do
  {- 1. Lexing  -}                                                                         
  (line, column, terminalListUptoCursor)  <-
    mainHaskellLexerWithLineColumn 1 1 programTextUptoCursor

  {- 2. Parsing -}
  ((do ast <- runAutomaton False 0
                      haskell_actionTable haskell_gotoTable haskell_prodRules
                      pFunList terminalListUptoCursor
       successfullyParsed)

    `catch` \parseError ->
      case parseError :: ParseError Token DummyAST of
        _ ->
          {- 3. Lexing the rest and computing candidates with it -}
          do (_, _, terminalListAfterCursor) <-
               mainHaskellLexerWithLineColumn line column programTextAfterCursor
             handleParseError debug isSimpleMode terminalListAfterCursor parseError))

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
