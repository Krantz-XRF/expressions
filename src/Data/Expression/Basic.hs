module Data.Expression.Basic
    ( Expression(..)
    , EvalExpr
    , EvalOp(..)
    , eval
    ) where

import GHC.Exts (Constraint)

-- | Generalized Expressions
-- Operators are abstracted out of definition of 'Expression'.
-- Only the recursive part is handled here.
data Expression op a
    = Atom a
    -- ^ The Leaf of a expression tree.
    | Op (op (Expression op a))
    -- ^ Recursive build trees using the provided @op@.
    deriving stock Functor

deriving stock instance (Show a, forall b . Show b => Show (op b)) => Show (Expression op a)
deriving stock instance (Eq a, forall b . Eq b => Eq (op b)) => Eq (Expression op a)

-- | Shortcut constraint
-- for @op@ and @a@ to be able to be evaluated.
type EvalExpr op a = (EvalOp op, CanEval op a)

-- | Whether the operator is able to be evaluated.
class EvalOp (op :: * -> *) where
    -- | 'CanEval' is the constraint on 'evalOp'.
    type CanEval op a :: Constraint
    type CanEval op a = ()
    -- | Evaluate the expression tree, whose root is @op@.
    -- Only one layer is handled here, others are handled recursively by 'eval'.
    evalOp :: (CanEval op a, EvalExpr op' a) => op (Expression op' a) -> a
    {-# MINIMAL evalOp #-}

-- | Eval the whole expression, with the help of 'EvalOp'.
-- Stop on leaf 'Atom', propagate to 'evalOp' on branch 'Op'.
eval :: EvalExpr op a => Expression op a -> a
eval (Atom x) = x
eval (Op op) = evalOp op