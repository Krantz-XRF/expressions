# Expressions

## expressions

Generalized operators and expressions.

Define operators (as `Functor`s) without explicit recursion, and get and expression tree for free. In light of `Free Monads`, this might be called a free expression tree. Just like free monads, since operators can be summed (at type level) together, and form another `Functor` as a sum operator, this expression tree is extensible.

## expressions-llvm

`expressions` targetting the LLVM backend.
