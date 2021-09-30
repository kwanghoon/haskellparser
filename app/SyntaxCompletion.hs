module SyntaxCompletion (computeCand, computeCandHaskell, CC_HaskellOption(..)) where

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
  computeCandHaskell debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode
    (CC_HaskellOption {
        vccurly_token = Nothing,
        nonterminalFormatFun = Nothing
        })

data CC_HaskellOption = CC_HaskellOption {
  vccurly_token :: Maybe Token,
  nonterminalFormatFun :: Maybe (String -> String)
  }

computeCandHaskell :: Bool -> Int -> String -> String -> Bool -> CC_HaskellOption -> IO [EmacsDataItem]
computeCandHaskell debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode cc_haskellOption = (do
  {- 1. Lexing  -}                                                                         
  (line, column, terminalListUptoCursor)  <-
    mainHaskellLexerWithLineColumn 1 1 programTextUptoCursor

  {- 2. Parsing -}
  ((do ast <- runAutomatonHaskell debug (AutomatonSpec { am_initState=0,
                      am_actionTbl=haskell_actionTable, am_gotoTbl=haskell_gotoTable, am_prodRules=haskell_prodRules,
                      am_parseFuns=pFunList}) terminalListUptoCursor
                      (vccurly_token cc_haskellOption)
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
                   nonterminalToStringMaybe=nonterminalFormatFun cc_haskellOption})  -- Just haskell_convFun
               parseError))

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
