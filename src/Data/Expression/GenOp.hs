{-# LANGUAGE UndecidableInstances #-}
module Data.Expression.GenOp
    ( GenOp(..)
    , GenExpression
    , HasOp(..)
    , HasOps
    , liftExpression
    ) where

import GHC.Exts (Constraint)

import Control.Applicative
import Data.Expression.Basic

-- | Generalized Operators:
-- @GenOp [<ops>...]@ is sum type of @<ops>...@.
data family GenOp (ops :: [* -> *]) (a :: *)
data instance GenOp '[] a = IdOp a deriving stock (Show, Eq, Functor)
data instance GenOp (x ': xs) a = HeadOp (x a)
                                | TailOps (GenOp xs a)

deriving stock instance (Eq (x a), Eq (GenOp xs a)) => Eq (GenOp (x ': xs) a)
deriving stock instance (Show (x a), Show (GenOp xs a)) => Show (GenOp (x ': xs) a)
deriving stock instance (Functor x, Functor (GenOp xs)) => Functor (GenOp (x ': xs))

-- | Generalized Expressions with a 'GenOp'.
type GenExpression (ops :: [* -> *]) = Expression (GenOp ops)

-- | Evaluation of 'GenOp' @[x]@
-- is propagated to the underlying operator @x@.
instance EvalOp (GenOp '[]) where
    type CanEval (GenOp '[]) a = ()
    evalOp (IdOp x) = eval x

-- | Evaluation of 'GenOp' @x ': xs@
-- is propagated to the underlying operators @x@ or @xs@ accordingly.
instance EvalOp (GenOp (x ': xs)) where
    type CanEval (GenOp (x ': xs)) a = (EvalExpr x a, EvalExpr (GenOp xs) a)
    evalOp (HeadOp m)   = evalOp m
    evalOp (TailOps ms) = evalOp ms

-- | Indicate that @genOp@ includes operator @op@.
class HasOp (op :: * -> *) (genOp :: * -> *) where
    liftOp :: op a -> genOp a
    checkOp :: genOp a -> Maybe (op a)
    {-# MINIMAL liftOp, checkOp #-}

-- | Lift a whole expression with the help of 'HasOp'.
liftExpression :: (Functor a, HasOp a b) => Expression a v -> Expression b v
liftExpression (Atom x) = Atom x
liftExpression (Op m) = Op $ liftOp $ fmap liftExpression m

-- | Shortcut constraint for 'HasOp'.
type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- | a is in (a : _)
instance {-# OVERLAPPING #-} HasOp a (GenOp (a ': xs)) where
    liftOp = HeadOp
    checkOp (HeadOp m) = Just m
    checkOp _ = Nothing

-- | a is in xs => a is in (x : xs)
instance {-# OVERLAPPABLE #-} HasOp a (GenOp xs)
    => HasOp a (GenOp (x ': xs)) where
    liftOp = TailOps . liftOp
    checkOp (TailOps ms) = checkOp ms
    checkOp _ = Nothing

-- | [] subsets []
instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp '[]) where
    liftOp  = id
    checkOp = Just

-- | [] subsets (_ : _)
instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp xs)
    => HasOp (GenOp '[]) (GenOp (x ': xs)) where
    liftOp = TailOps . liftOp
    checkOp (HeadOp _) = Nothing
    checkOp (TailOps m) = checkOp m

-- | x is in g, xs is in g => (x : xs) is in g
instance {-# OVERLAPPING #-}
    (HasOp x (GenOp (y ': ys)), HasOp (GenOp xs) (GenOp (y ': ys)))
    => HasOp (GenOp (x ': xs)) (GenOp (y ': ys)) where
    liftOp (HeadOp m)   = liftOp m
    liftOp (TailOps ms) = liftOp ms
    checkOp g = HeadOp <$> checkOp @x g
              <|> TailOps <$> checkOp @(GenOp xs) g