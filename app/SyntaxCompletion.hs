module SyntaxCompletion (computeCand, computeCandHaskell) where

import Lexer(Token(..))

import HaskellLexer
import HaskellParser
import HaskellAST
import HaskellToken
import Terminal
import HaskellParserUtil

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
  ((do ast <- runAutomatonHaskell debug (AutomatonSpec { am_initState=0,
                      am_actionTbl=haskell_actionTable, am_gotoTbl=haskell_gotoTable, am_prodRules=haskell_prodRules,
                      am_parseFuns=pFunList}) terminalListUptoCursor
                      haskellOption
       successfullyParsed)

    `catch` \parseError ->
      case parseError :: ParseError Token DummyAST of
        _ ->
          {- 3. Lexing the rest and computing candidates with it -}
          do (_, _, terminalListAfterCursor) <-
               mainHaskellLexerWithLineColumn line column programTextAfterCursor
             handleParseError
               (HandleParseError {
                   debugFlag=debug,
                   searchMaxLevel=maxLevel,
                   simpleOrNested=isSimpleMode,
                   postTerminalList=terminalListAfterCursor,
                   nonterminalToStringMaybe=Just haskell_convFun})
               parseError))

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
