module HaskellAST where

import Data.Typeable

data DummyAST = DummyAST  -- Just for dummy AST
  deriving (Show, Typeable)

parseFun rhs = DummyAST

pFunList = parseFun : pFunList
