module Data.Expression.GenOp
    ( GenOp
    , GenOpI(..)
    , GenExpression
    , HasOp(..)
    , HasOps
    , liftExpression
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

-- | Generalized Expressions with a 'GenOp'.
type GenExpression (ops :: [* -> *]) = Expression (GenOp ops)

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

liftExpression :: (Functor a, HasOp a b) => Expression a v -> Expression b v
liftExpression (Atom x) = Atom x
liftExpression (Op m) = Op $ liftOp $ fmap liftExpression m

-- | Shortcut constraint for 'HasOp'.
type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- | a is in [a]
instance {-# OVERLAPPABLE #-} HasOp a (GenOpI ('Last a)) where
    liftOp = TailTipOp
    checkOp (TailTipOp m) = Just m

-- | a is in (a : _)
instance {-# OVERLAPS #-} HasOp a (GenOpI ('Cons a xs)) where
    liftOp = HeadOp
    checkOp (HeadOp m) = Just m
    checkOp _ = Nothing

-- | a is in xs => a is in (x : xs)
instance {-# OVERLAPPABLE #-} HasOp a (GenOpI xs)
    => HasOp a (GenOpI ('Cons x xs)) where
    liftOp = TailOps . liftOp
    checkOp (TailOps ms) = checkOp ms
    checkOp _ = Nothing

-- | x is in (y : ys) => [x] subsets (y : ys)
instance {-# OVERLAPPING #-} HasOp x (GenOpI ('Cons y ys))
    => HasOp (GenOpI ('Last x)) (GenOpI ('Cons y ys)) where
    liftOp (TailTipOp m) = liftOp m
    checkOp g = TailTipOp <$> checkOp @x g

-- | [x] subsets [x]
instance {-# OVERLAPPING #-} HasOp (GenOpI ('Last x)) (GenOpI ('Last x)) where
    liftOp  = id
    checkOp = Just

-- | x is in (y : ys), xs is in (y : ys) => (x : xs) subsets (y : ys)
instance {-# OVERLAPPING #-}
    (HasOp x (GenOpI ('Cons y ys)), HasOp (GenOpI xs) (GenOpI ('Cons y ys)))
    => HasOp (GenOpI ('Cons x xs)) (GenOpI ('Cons y ys)) where
    liftOp (HeadOp m)   = liftOp m
    liftOp (TailOps ms) = liftOp ms
    checkOp g = liftOp <$> checkOp @x g
              <|> TailOps <$> checkOp @(GenOpI xs) g