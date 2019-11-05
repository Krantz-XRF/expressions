module Data.Expression.GenOp
    ( GenOp
    , GenOpI(..)
    , HasOp(..)
    , HasOps
    ) where

import GHC.Exts (Constraint)

import Control.Applicative
import Data.Expression.Basic
import Data.Expression.Utility

-- | Generalized Operators:
-- @GenOp [ops...]@ is sum type of @ops...@.
type GenOp (ops :: [* -> *]) = GenOpI (MkNonEmpty ops)

-- | Implementation for generalized operators.
data family GenOpI (ops :: NonEmpty (* -> *)) (a :: *)
data instance GenOpI ('Last x) a = TailTipOp (x a) deriving stock (Show, Eq, Functor)
data instance GenOpI ('Cons x xs) a
    = HeadOp (x a)
    | TailOps (GenOpI xs a)

deriving stock instance (Eq (x a), Eq (GenOpI xs a)) => Eq (GenOpI ('Cons x xs) a)
deriving stock instance (Show (x a), Show (GenOpI xs a)) => Show (GenOpI ('Cons x xs) a)
deriving stock instance (Functor x, Functor (GenOpI xs)) => Functor (GenOpI ('Cons x xs))

-- | Evaluation of 'GenOp' @[x]@
-- is propagated to the underlying operator @x@.
instance EvalOp x => EvalOp (GenOpI ('Last x)) where
    type CanEval (GenOpI ('Last x)) a = CanEval x a
    evalOp (TailTipOp e) = evalOp e

-- | Evaluation of 'GenOp' @x ': xs@
-- is propagated to the underlying operators @x@ or @xs@ accordingly.
instance EvalOp (GenOpI ('Cons x xs)) where
    type CanEval (GenOpI ('Cons x xs)) a = (EvalExpr x a, EvalExpr (GenOpI xs) a)
    evalOp (HeadOp m)   = evalOp m
    evalOp (TailOps ms) = evalOp ms

-- | Indicate that @genOp@ includes operator @op@.
class HasOp (op :: * -> *) (genOp :: * -> *) where
    liftOp :: op a -> genOp a
    checkOp :: genOp a -> Maybe (op a)
    {-# MINIMAL liftOp, checkOp #-}

-- | Shortcut constraint for 'HasOp'.
type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- | a is in (a : _)
instance {-# OVERLAPPING #-} HasOp a (GenOpI ('Cons a xs)) where
    liftOp = HeadOp
    checkOp (HeadOp m) = Just m
    checkOp _ = Nothing

-- | a is in xs => a is in (x : xs)
instance {-# OVERLAPPABLE #-} HasOp a (GenOpI xs)
    => HasOp a (GenOpI ('Cons x xs)) where
    liftOp = TailOps . liftOp
    checkOp (TailOps ms) = checkOp ms
    checkOp _ = Nothing

-- | x is in xs => [x] subsets xs
instance {-# OVERLAPPING #-} HasOp x (GenOpI xs)
    => HasOp (GenOpI ('Last x)) (GenOpI xs) where
    liftOp (TailTipOp m) = liftOp m
    checkOp g = TailTipOp <$> checkOp @x g

-- | x is in g, xs is in g => (x : xs) is in g
instance (HasOp x genOp, HasOp (GenOpI xs) genOp)
    => HasOp (GenOpI ('Cons x xs)) genOp where
    liftOp (HeadOp m)   = liftOp m
    liftOp (TailOps ms) = liftOp ms
    checkOp g = liftOp <$> checkOp @x g
              <|> TailOps <$> checkOp @(GenOpI xs) g