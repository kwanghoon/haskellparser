module HaskellLexer (haskellLexer, haskellLexerWithLineColumn) where

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
import SrcLoc (Located, GenLocated(..), SrcSpan(..), unLoc
              , mkRealSrcLoc, mkRealSrcSpan
              , srcSpanStartLine, srcSpanStartCol, srcSpanEndLine, srcSpanEndCol)
import StringBuffer
import ToolSettings

import Debug.Trace
import Data.Either

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

  newtype P a = P { unP :: PState -> ParseResult a }

-}


--
haskellLexer :: String -> IO [Terminal Token]
haskellLexer str = do
  (_,_,tokens) <- haskellLexerWithLineColumn 1 1 str
  return tokens
  
haskellLexerWithLineColumn :: Line -> Column -> String -> IO (Line, Column, [Terminal Token])
haskellLexerWithLineColumn line0 col0 str = do
  case runHaskellLexerWithLineColumn line0 col0 str of
    POk parseState (line, col, ss)  ->
      return (line, col, ss ++ [ Terminal (fromToken ITeof) line col (Just ITeof) ])
    PFailed parseState -> return (0, 0, []) -- putStrLn "PFailed..."

-- Actually, run a lexer
runHaskellLexer :: String -> ParseResult (Line, Column, [Terminal Token])
runHaskellLexer str = runHaskellLexerWithLineColumn 1 1 str

runHaskellLexerWithLineColumn :: Line -> Column -> String -> ParseResult (Line, Column, [Terminal Token])
runHaskellLexerWithLineColumn line col str = unP allHaskellTokens (initParseState line col str)

initParseState line col str = parseState
  where
    filename = "<interactive>"
    location = mkRealSrcLoc (mkFastString filename) line col
    buffer = stringToStringBuffer str
    parseState = mkPState dynFlags buffer location

-- Haskell lexer getting all tokens until ITeof
type MyRealSrcSpan = (Int,Int,Int,Int)

type Line = Int
type Column = Int

lexerDbg queueComments cont = lexer queueComments contDbg
  where
    contDbg tok = trace ("token: " ++ show (unLoc tok)) (cont tok)

singleHaskellToken :: P (Located Token)
singleHaskellToken =
  Lexer.lexer False
  -- lexerDbg False
    (\locatedToken -> P (\pstate -> POk pstate locatedToken))

-- Converting a located token into either a terminal or the location of ITeof:
toTerminalToken :: Located Token -> Either (Terminal Token) (Line, Column)
toTerminalToken locatedToken =
  case locatedToken of
    L srcspan tok ->
      let { (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan } in  -- Todo: hslexer parse error without { and }
        case tok of 
          ITeof -> Right (end_line, end_col)
          _ -> Left $ Terminal (hiddenText tok) start_line start_col (Just tok)
    
allHaskellTokens :: P (Line, Column, [Terminal Token])
allHaskellTokens = do
    (line, col, ss) <- tokInfos []  -- Todo: hslexer parse error withtout the indentation '  ' before '('
    return (line, col, reverse ss)
  where
    tokInfos :: [Terminal Token] -> P (Line, Column, [Terminal Token])
    tokInfos s = do
      locatedToken <- singleHaskellToken
      case toTerminalToken locatedToken of
        Left terminal -> tokInfos (terminal : s)
        Right (end_line, end_col) -> return (end_line, end_col, s)

-- Util
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
