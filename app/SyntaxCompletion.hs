module SyntaxCompletion (computeCand) where

import Lexer(Token(..))

import HaskellLexer
import HaskellParser
import HaskellParserUtil(haskell_convFun)
import HaskellAST
import HaskellToken
import Terminal
import HaskellFilter

import ParserTime

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

maxLevel = 1

-- | computeCand
computeCand :: Bool -> String -> String -> Bool -> IO [EmacsDataItem]
computeCand debug programTextUptoCursor programTextAfterCursor isSimpleMode =
  {- 1. Parsing -}
  ((do ast <- runAutomaton debug
                (AutomatonSpec {
                    am_initState=initState,
                    am_actionTbl=haskell_actionTable,
                    am_gotoTbl=haskell_gotoTable,
                    am_prodRules=haskell_prodRules,
                    am_parseFuns=pFunList,
                    am_time=AutomatonTime{
                     am_startTime=startTime,
                     am_finishTime=finishTime,
                     am_cputime=0
                                }  })
                
                (initParseState 1 1 programTextUptoCursor,1,1,programTextUptoCursor)
                aHaskellLexer          -- (aLexer lexerSpec)

       successfullyParsed)

       `catch` \parseError ->
         case parseError :: ParseError Token DummyAST HaskellParseState of
           _ ->
             {- 2. Lexing the rest and computing candidates with it -}
             do let (_,line,column,programTextAfterCursor) = lpStateFrom parseError
                compCandidates <- chooseCompCandidatesFn

                let lexerSpec = undefined                              -- 
                let parserSpec = ParserSpec { synCompSpec = Just (SynCompSpec {isAbleToSearch=canSearch}) }  -- partially defined
                      
                handleParseError
                  compCandidates
                  (defaultHandleParseError lexerSpec parserSpec) {
                      debugFlag=debug,
                      searchMaxLevel=maxLevel,
                      simpleOrNested=isSimpleMode,
                      postTerminalList=[],       -- terminalListAfterCursor,
                      nonterminalToStringMaybe=Just haskell_convFun}  -- Just haskell_convFun
                  parseError)

  `catch` \lexError ->  case lexError :: LexError of  _ -> handleLexError
