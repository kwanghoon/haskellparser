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
          let (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan 
          in  return (end_line, end_col, s)
          
        L srcspan tok ->
          let (start_line, start_col, end_line, end_col) = srcSpanToLineCol srcspan 
          in  tokInfos (Terminal (hiddenText tok) start_line start_col (Just tok) : s)

    srcSpanToLineCol :: SrcSpan -> MyRealSrcSpan
    srcSpanToLineCol (RealSrcSpan realSrcSpan') =
      (srcSpanStartLine realSrcSpan', srcSpanStartCol realSrcSpan'
      ,srcSpanEndLine realSrcSpan', srcSpanEndCol realSrcSpan')
    srcSpanToLineCol (UnhelpfulSpan _) = (0,0,0,0)
