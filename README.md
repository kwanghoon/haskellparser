
# Syntax complection for Haskell Using YAPB

```
$ stack build

$ stack exec -- lexer-exe ./examples/HelloWorld.hs  (for parsing with an LALR(1) automaton, not building syntactic trees)

$ stack exec -- lexer-exe test   (for testing syntax complection)

$ stack exec -- lexer-exe emacs  (for running emacs server)
```

### Note
- YAPB-0.1.3 available for Hackage, stack build should build this project. In case it does not, you can download yapb-0.1.3 from GitHub into your local directory and can refer it in stack.yaml. 
- SyntaxCompletionSpec.hs for Hspec based Unit testing. You can easily add or change testcases through editing this file. 