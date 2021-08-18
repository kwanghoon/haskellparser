module HaskellLexer (mainHaskellLexer, mainHaskellLexerWithLineColumn) where

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


--
mainHaskellLexer :: String -> IO [Terminal Token]
mainHaskellLexer str = do
  case runHaskellLexer str of
    POk parseState (line, col, ss)  ->
      return (ss ++ [ Terminal (fromToken ITvccurly) line col (Just ITvccurly)
                    , Terminal (fromToken ITeof) line col (Just ITeof) ])
    PFailed parseState -> return [] -- putStrLn "PFailed..."

mainHaskellLexerWithLineColumn :: Line -> Column -> String -> IO (Line, Column, [Terminal Token])
mainHaskellLexerWithLineColumn line0 col0 str = do
  case runHaskellLexer str of
    POk parseState (line, col, ss)  ->
      return (line+line0-1, col+col0-1,
               map (\(Terminal text l c tok) -> Terminal text (l+line0-1) (c+col0-1) tok)
                 (ss  ++ [ Terminal (fromToken ITeof) line col (Just ITeof) ]))
    PFailed parseState -> return (0, 0, []) -- putStrLn "PFailed..."

-- Actually, run a lexer
runHaskellLexer :: String -> ParseResult (Line, Column, [Terminal Token])
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

type Line = Int
type Column = Int

haskellLexer :: P (Line, Column, [Terminal Token])
haskellLexer = do
  (line, col, ss) <- tokInfos []
  return (line, col, reverse ss)
  where
    -- Haskell lexer getting a single token
    singleHaskellToken :: P (Located Token)
    singleHaskellToken =
      Lexer.lexer False
        (\locatedToken -> P (\pstate -> POk pstate locatedToken))

    tokInfos :: [Terminal Token] -> P (Line, Column, [Terminal Token])
    tokInfos s = do
      locatedToken <- singleHaskellToken
      case locatedToken of
        L srcspan ITeof ->
          let (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan in
          return (end_line, end_col, s)
          
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
