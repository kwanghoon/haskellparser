module Main where

-- [NOTE]
-- To use modules in ghc-parser-lib package,
-- refer to https://www.stackage.org/nightly-2019-09-04/package/ghc-lib-parser-8.8.0.20190424
--
-- For example, import Lexer, not import GHC.Parser.Lexer. 

import DynFlags
import FastString
import Fingerprint
import GHC.Platform
import Lexer
import PlatformConstants
import SrcLoc
import StringBuffer
import ToolSettings

{-
Given

 do { x <- m; return x }

this haskell lexer produces a list of
tokens and line and column information.

$ stack exec lexer-exe
(1,1,1,3): ITdo
(1,4,1,5): ITocurly
(1,6,1,7): ITvarid "x"
(1,8,1,10): ITlarrow NormalSyntax
(1,11,1,12): ITvarid "m"
(1,12,1,13): ITsemi
(1,14,1,20): ITvarid "return"
(1,21,1,22): ITvarid "x"
(1,23,1,24): ITccurly
-}

--
main :: IO ()
main = do
  let settings = defaultSettings
  let llvmConfig = defaultLlvmConfig
  let dynFlags = defaultDynFlags settings llvmConfig
  
  case runParser dynFlags "do { x <- m; return x }" toks of
    POk _ ss -> mapM_ (\(srcspan,tok) -> do putStr (show srcspan++": "); putStrLn tok) ss
    PFailed _ -> putStrLn "PFailed..."

--
runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState flags buffer location

hsLex :: P (Located Token)
hsLex = Lexer.lexer False  (\locatedToken -> P (\pstate -> POk pstate locatedToken))

toks :: P [((Int,Int,Int,Int),String)]
toks = do
  ss <- toks' []
  return (reverse ss)

toks' :: [((Int,Int,Int,Int),String)] -> P [((Int,Int,Int,Int),String)]
toks' s = do
  locatedToken <- hsLex
  case locatedToken of
    L _ ITeof -> return s
    L srcspan tok ->
      toks' ((srcSpanToLineCol srcspan, show tok) : s)

srcSpanToLineCol :: SrcSpan -> (Int, Int, Int, Int)
srcSpanToLineCol (RealSrcSpan realSrcSpan') =
  (srcSpanStartLine realSrcSpan', srcSpanStartCol realSrcSpan'
  ,srcSpanEndLine realSrcSpan', srcSpanEndCol realSrcSpan')
srcSpanToLineCol (UnhelpfulSpan _) = (0,0,0,0)

--
defaultSettings =
  Settings {
   sGhcNameVersion = GhcNameVersion "" "",
   sFileSettings = FileSettings "" "" Nothing "" "" "",
   sTargetPlatform =
     Platform (PlatformMini ArchUnknown OSUnknown) PW8 False False False False False,
   sToolSettings =
     ToolSettings
       False False False False False
       "" ("",[]) "" "" ("",[]) ("",[]) ("",[]) ("",[]) "" "" "" "" "" "" ""
       ("",[]) ("",[]) ("",[])
       "" [] [] (Fingerprint 123 456) [] [] [] [] [] [] [] [] [] [] [] [] ,
   sPlatformMisc =
     PlatformMisc
       "" "" IntegerSimple False False False "" False
       False False False False False "",
   sPlatformConstants =
     PlatformConstants
       0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0 
       0 0 0 0 0 0 0 0 0 0
       0 0 0 0 0 False False 0 0 0 0,
    sRawSettings = []
  }
    

defaultLlvmConfig = LlvmConfig [] []
