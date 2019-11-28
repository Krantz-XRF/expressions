{-# LANGUAGE ConstraintKinds #-}
module Data.Expression.Basic
    ( Expression(..)
    , EvalExprM
    , EvalExpr
    , EvalOpM(..)
    , evalM
    , EvalOp
    , CanEval
    , eval
    ) where

import Data.Functor.Identity
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
-- for @op@ and @a@ to be able to be evaluated in the 'Identity' monad.
type EvalExpr op a = EvalExprM Identity op a

-- | Shortcut constraint
-- for @op@ and @a@ to be able to be evaluated.
type EvalExprM m op a = (EvalOpM m op, CanEvalM m op a)

-- | Whether the @op@ can be evaluated in the 'Identity' monad.
type EvalOp op = EvalOpM Identity op

-- | Constraint for being evaluated in 'Identity' monad.
type CanEval op a = CanEvalM Identity op a

-- | Whether the operator is able to be evaluated.
class Monad m => EvalOpM (m :: * -> *) (op :: * -> *) where
    -- | 'CanEval' is the constraint on 'evalOp'.
    type CanEvalM m op a :: Constraint
    type CanEvalM m op a = ()
    -- | Evaluate the expression tree, whose root is @op@.
    -- Only one layer is handled here, others are handled recursively by 'eval'.
    evalOp :: (CanEvalM m op a, EvalExprM m op' a) => op (Expression op' a) -> m a
    {-# MINIMAL evalOp #-}

-- | Eval the whole expression, with the help of 'EvalOp'.
-- Stop on leaf 'Atom', propagate to 'evalOp' on branch 'Op'.
evalM :: EvalExprM m op a => Expression op a -> m a
evalM (Atom x) = return x
evalM (Op op) = evalOp op

-- | Eval the whole expression in 'Identity' monad.
eval :: EvalExpr op a => Expression op a -> a
eval = runIdentity . evalM