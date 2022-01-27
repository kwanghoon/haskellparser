module SyntaxCompletion (computeCand) where

import Lexer(Token(..))

import HaskellLexer
import HaskellParser
import HaskellParserUtil(haskell_convFun)
import HaskellAST
import HaskellToken
import Terminal

import CommonParserUtil 

import System.IO

-- for syntax completion
import SynCompInterface
import SynCompAlgorithm
import Control.Exception

-- Todo: The following part should be moved to the library.
--       Arguments: lexerSpec, parserSpec
--                  isSimpleMode
--                  programTextUptoCursor, programTextAfterCursor

-- | computeCand
computeCand :: Bool -> Int -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug maxLevel programTextUptoCursor programTextAfterCursor isSimpleMode =
  {- 1. Parsing -}
  ((do ast <- runAutomaton debug
                (AutomatonSpec {
                    am_initState=initState,
                    am_actionTbl=haskell_actionTable,
                    am_gotoTbl=haskell_gotoTable,
                    am_prodRules=haskell_prodRules,
                    am_parseFuns=pFunList})
                (initParseState 1 1 programTextUptoCursor,1,1,programTextUptoCursor)
                aHaskellLexer          -- (aLexer lexerSpec)

       successfullyParsed)

       `catch` \parseError ->
         case parseError :: ParseError Token DummyAST HaskellParseState of
           _ ->
             {- 2. Lexing the rest and computing candidates with it -}
             do let (_,line,column,programTextAfterCursor) = lpStateFrom parseError
                compCandidates <- chooseCompCandidatesFn
                
                handleParseError
                  compCandidates
                  (defaultHandleParseError {
                      debugFlag=debug,
                      searchMaxLevel=maxLevel,
                      simpleOrNested=isSimpleMode,
                      postTerminalList=[],       -- terminalListAfterCursor,
                      nonterminalToStringMaybe=Just haskell_convFun})  -- Just haskell_convFun
                  parseError)

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
