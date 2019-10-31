module Data.Expression.Basic
    ( Expression(..)
    , EvalExpr
    , EvalOp(..)
    , eval
    ) where

import GHC.Exts (Constraint)

data Expression op a
    = Atom a
    | Op (op (Expression op a))
    deriving stock Functor

deriving stock instance (Show a, forall b . Show b => Show (op b)) => Show (Expression op a)
deriving stock instance (Eq a, forall b . Eq b => Eq (op b)) => Eq (Expression op a)

type EvalExpr op a = (EvalOp op, CanEval op a)

class EvalOp (op :: * -> *) where
    type CanEval op a :: Constraint
    type CanEval op a = ()
    evalOp :: (CanEval op a, EvalExpr op' a) => op (Expression op' a) -> a
    {-# MINIMAL evalOp #-}

eval :: EvalExpr op a => Expression op a -> a
eval (Atom x) = x
eval (Op op) = evalOp op