# Changelog for expressions

The bullet points marked a :warning: are interface-breaking changes. By 'interface-breaking', we mean some well-formed code may break after this change.

## Unreleased changes

- Introduced an extra `Monad` parameter in `EvalOp`
  - Rename `EvalOp` as `EvalOpM`
  - `EvalOp` re-introduced as an alias for `EvalOp Identity`
- :warning: Swapped the patterns `O<op>` and `<op>` in Template-Haskell generated code
  - `O<op>` was intended as a shortcut for `Op . <op>`
- Relaxed the type signature for `<op>`
  - Take `Plus` as an example
  - Previously `Plus :: HasOp MonoidOp op => Expression op a -> Expression op a -> op (Expression op a)`
  - Now `Plus :: HasOp MonoidOp op => a -> a -> op a`
  - This allows more flexible use of the operators
- Now `HasOp` supports `HasOp x x`
  - This enables the use of e.g. `Plus` instead of `MPlus`
  - Before this change, since `Plus` requires `HasOp MonoidOp op`, and there is no `HasOp MonoidOp MonoidOp`
  - Note that, before this change, we already had `HasOp x x` for `x ~ GenOp ops`

## v0.1

- First version of this library, now we support creation (`makeOp`), lifting (`liftOp`/`liftExpression`), extracting (`checkOp`), and evaluation (`eval`).
