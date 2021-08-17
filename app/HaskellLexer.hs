module HaskellLexer (mainHaskellLexer) where

-- [NOTE]
-- To use modules in ghc-parser-lib package,
-- refer to https://www.stackage.org/nightly-2019-09-04/package/ghc-lib-parser-8.8.0.20190424
--
-- For example, import Lexer, not import GHC.Parser.Lexer.

import Terminal
import TokenInterface
import HaskellToken

import DynFlags
import FastString
import Fingerprint
import GHC.Platform
import Lexer (Token(..), lexer, mkPState, P(..), PState, ParseResult(..))
import PlatformConstants
import SrcLoc (Located, GenLocated(..), SrcSpan(..)
              , mkRealSrcLoc, mkRealSrcSpan
              , srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import StringBuffer
import ToolSettings


{-
[Lexer]

  data Token =
    ITas |
    ITcase |
    ITdata |
    ...

  lexer :: Bool -> (Located Token -> P a) -> P a

  mkPState :: DynFlags -> StringBuffer -> RealSrcLoc -> PState

  data Token

  data P a = P { unP :: PState -> ParseResult a }

  data PState = PState
   buffer :: StringBuffer
   options :: ParserFlags
   ...

  data ParseResult a =
   POk PState a |
   PFailed PState         -- (DyNFlags -> Messages) SrcSpan MsgDoc

[SrcLoc]

  mkRealSrcLoc :: FastString -> Int -> Int -> RealSrcLoc

  mkRealSrcSpan :: RealSrcLoc -> RealSrcLoc -> RealSrcSpan

  srcSpanStartLine :: RealSrcSpan -> Int

  srcSpanEndLine :: RealSrcSpan -> Int

  srcSpanStartCol :: RealSrcSpan -> Int

  srcSpanEndCol :: RealSrcSpan -> Int

  data RealSrcSpan
    = RealSrcSpan'
          { srcSpanFile     :: !FastString,
            srcSpanSLine    :: {-# UNPACK #-} !Int,
            srcSpanSCol     :: {-# UNPACK #-} !Int,
            srcSpanELine    :: {-# UNPACK #-} !Int,
            srcSpanECol     :: {-# UNPACK #-} !Int
          }
    deriving Eq

-}

{-
[Running]

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
mainHaskellLexer :: String -> IO [Terminal Token]
mainHaskellLexer str = do
  case runHaskellLexer str of
    POk parseState ss  -> return ss
    PFailed parseState -> return [] -- putStrLn "PFailed..."

-- Actually, run a lexer
runHaskellLexer :: String -> ParseResult [Terminal Token]
runHaskellLexer str = unP haskellLexer parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) 1 1
    buffer = stringToStringBuffer str
    parseState = mkPState dynFlags buffer location

-- Haskell lexer getting all tokens until ITeof
type MyRealSrcSpan = (Int,Int,Int,Int)
-- type TokenInfo = (MyRealSrcSpan,String)

-- prTokInfos :: [TokenInfo] -> IO ()
-- prTokInfos ss =
--   mapM_
--     (\(srcSpan,tokName) -> do
--         putStr (show srcSpan ++ ": ")
--         putStrLn tokName)
--     ss

haskellLexer :: P [Terminal Token]
haskellLexer = do
  ss <- tokInfos []
  return (reverse ss)
  where
    -- Haskell lexer getting a single token
    singleHaskellToken :: P (Located Token)
    singleHaskellToken =
      Lexer.lexer False
        (\locatedToken -> P (\pstate -> POk pstate locatedToken))

    tokInfos :: [Terminal Token] -> P [Terminal Token]
    tokInfos s = do
      locatedToken <- singleHaskellToken
      case locatedToken of
        L srcspan ITeof ->
          let (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan in
          return (Terminal (fromToken ITeof) start_line start_col (Just ITeof) : s)
          
        L srcspan tok ->
          -- tokInfos ((srcSpanToLineCol srcspan, tok) : s)
          let (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan in
          tokInfos (Terminal (fromToken tok) start_line start_col (Just tok) : s)

    srcSpanToLineCol :: SrcSpan -> MyRealSrcSpan
    srcSpanToLineCol (RealSrcSpan realSrcSpan') =
      (srcSpanStartLine realSrcSpan', srcSpanStartCol realSrcSpan'
      ,srcSpanEndLine realSrcSpan', srcSpanEndCol realSrcSpan')
    srcSpanToLineCol (UnhelpfulSpan _) = (0,0,0,0)

-- GHC flags necessary for Lexer.lexer to build parser states
-- Made a dummy flag!
dynFlags :: DynFlags
dynFlags = defaultDynFlags settings llvmConfig
 where
  settings = defaultSettings
  llvmConfig = defaultLlvmConfig

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
