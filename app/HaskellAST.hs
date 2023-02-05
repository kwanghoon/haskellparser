module HaskellAST where

import CommonParserUtil

import Lexer (Token(..), mkPState, P(..), PState, ParseResult(..), popContext)
import Data.Typeable
import qualified Control.Monad.Trans.State.Lazy as ST
import Control.Monad.Trans.Class
import Control.Exception

data DummyAST = DummyAST  -- Just for dummy AST
  deriving (Show, Typeable)

parseFun :: ParseAction Token DummyAST IO PState
-- parseFun :: Stack Token DummyAST -> ST.StateT (LexerParserState PState) IO DummyAST
parseFun rhs = return DummyAST

pFunList = take 805 pFunList' ++ [actionCloseError] ++ take 10 pFunList'
  where
    pFunList' = parseFun : pFunList'

actionCloseError :: ParseAction Token DummyAST IO PState
actionCloseError stk =
  do (parseState,line,col,text) <- ST.get
  
     case unP popContext parseState of   -- close -> error { popContext() }
       POk nextParseState () ->
         do ST.put (nextParseState,line,col,text)
            return DummyAST
            
       PFailed nextParseState ->
         do ST.put (nextParseState,line,col,text)
            throw (LexError line col "[actionCloseError] lex error")

