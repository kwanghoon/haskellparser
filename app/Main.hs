module Main where

import DynFlags
import FastString
import Fingerprint
import GHC.Platform
import Lexer
import PlatformConstants
import SrcLoc
import StringBuffer
import ToolSettings

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
