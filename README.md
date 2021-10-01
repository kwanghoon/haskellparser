
# Syntax complection for Haskell Using YAPB

```
$ stack build

$ stack exec -- lexer-exe ./examples/HelloWorld.hs  (for parsing with an LALR(1) automaton, not building syntactic trees)

$ stack exec -- lexer-exe candidate ./examples/spec/hscode1  (for printing syntax completion candidates)

$ stack exec -- lexer-exe candidateinfo ./examples/spec/hscode1  (for for printing syntax completion candidates with terminal and noterminal symbol names)

$ stack exec -- lexer-exe test   (for testing syntax complection)

$ stack exec -- lexer-exe emacs  (for running emacs server)
```

### Note
- This project depends on YAPB-0.1.3.1 available for Hackage. 
- SyntaxCompletionSpec.hs for Hspec based Unit testing. You can easily add or change testcases through editing this file. 
