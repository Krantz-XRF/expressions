module Data.Expression.GenOp
    ( GenOp(..)
    , HasOp(..)
    , HasOps
    ) where

import GHC.Exts (Constraint)

import Control.Applicative
import Data.Expression.Basic

data family GenOp (ops :: [* -> *]) (a :: *)
data instance GenOp '[] a = NullOp deriving stock (Show, Eq, Functor)
data instance GenOp (x ': xs) a = HeadOp (x a)
                                | TailOps (GenOp xs a)

deriving stock instance (Eq (x a), Eq (GenOp xs a)) => Eq (GenOp (x ': xs) a)
deriving stock instance (Show (x a), Show (GenOp xs a)) => Show (GenOp (x ': xs) a)
deriving stock instance (Functor x, Functor (GenOp xs)) => Functor (GenOp (x ': xs))

instance EvalOp (GenOp '[]) where
    type CanEval (GenOp '[]) a = Num a
    evalOp _ = 0

instance EvalOp (GenOp (x ': xs)) where
    type CanEval (GenOp (x ': xs)) a = (EvalExpr x a, EvalExpr (GenOp xs) a)
    evalOp (HeadOp m)   = evalOp m
    evalOp (TailOps ms) = evalOp ms

class HasOp (op :: * -> *) (genOp :: * -> *) where
    liftOp :: op a -> genOp a
    checkOp :: genOp a -> Maybe (op a)
    {-# MINIMAL liftOp, checkOp #-}

type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- a is in (a : _)
instance {-# OVERLAPPING #-} HasOp a (GenOp (a ': xs)) where
    liftOp = HeadOp
    checkOp (HeadOp m) = Just m
    checkOp _ = Nothing

-- a is in xs => a is in (x : xs)
instance {-# OVERLAPPABLE #-} HasOp a (GenOp xs) => HasOp a (GenOp (x ': xs)) where
    liftOp = TailOps . liftOp
    checkOp (TailOps ms) = checkOp ms
    checkOp _ = Nothing

-- [] subsets []
instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp '[]) where
    liftOp  = const NullOp
    checkOp = const Nothing

-- [] subsets (_ : _)
instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp (x ': xs)) where
    liftOp  = const (liftOp NullOp)
    checkOp = const Nothing

-- x is in g, xs is in g => (x : xs) is in g
instance {-# OVERLAPPING #-} (HasOp x genOp, HasOp (GenOp xs) genOp)
    => HasOp (GenOp (x ': xs)) genOp where
    liftOp (HeadOp m)   = liftOp m
    liftOp (TailOps ms) = liftOp ms
    checkOp g = liftOp <$> checkOp @x g
              <|> TailOps <$> checkOp @(GenOp xs) g