# Lambda Calculus Interpreter

An interpreter for the untyped lambda calculus, written in Haskell. It parses lambda expressions from text, reduces them step by step under either normal-order or applicative-order evaluation, supports named macros that can be defined interactively, and includes a second, nameless representation of terms based on De Bruijn indices. Everything is exposed through a small REPL.

The project was developed as an assignment for the Programming Paradigms course at POLITEHNICA Bucharest (2026). Its purpose is to study, by building them, the mechanisms that every functional language relies on underneath: substitution, variable binding, evaluation order, and the representation of programs as data.

## Features

- an abstract syntax for lambda terms (variables, abstractions, applications, macros), with equality up to alpha-equivalence
- capture-avoiding substitution, with automatic renaming of bound variables
- single-step reduction under normal order and applicative order, and the full reduction sequence of a term down to its normal form
- a parser for lambda expressions built from first principles as a monadic parser combinator library, with no external parsing package
- macros: named terms that are expanded before evaluation, with a default set of classic combinators (`I`, `K`, `KI`, `M`, `C`, `Y`)
- a De Bruijn representation of terms, with conversion in both directions and its own reduction engine
- an interactive REPL and a graded test suite

## Project structure

```
Lambda.hs      term representation, substitution, reduction strategies
Parser.hs      parser combinator library and the lambda expression grammar
Code.hs        contexts, macro expansion, evaluation with error handling
Default.hs     the default context of combinators
DeBruijn.hs    nameless representation and its reduction engine
main.hs        REPL
Tests/         test suite and grader
```

