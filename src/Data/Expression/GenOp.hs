{-# LANGUAGE UndecidableInstances #-}
module Data.Expression.GenOp
    ( GenOp(..)
    , GenExpression
    , HasOp
    , liftOp
    , checkOp
    , HasOps
    , liftExpression
    ) where

import GHC.Exts (Constraint)

import Data.Proxy
import Unsafe.Coerce

-- import Control.Applicative
import Data.Expression.Basic
import Data.Expression.Utility

-- | Generalized Operators:
-- @GenOp [<ops>...]@ is sum type of @<ops>...@.
data family GenOp (ops :: [* -> *]) (a :: *)
data instance GenOp '[] a = IdOp a deriving stock (Show, Eq, Functor)
data instance GenOp (x ': xs) a = HeadOp (x a)
                                | TailOps (GenOp xs a)

deriving stock instance (Eq (x a), Eq (GenOp xs a)) => Eq (GenOp (x ': xs) a)
deriving stock instance (Show (x a), Show (GenOp xs a)) => Show (GenOp (x ': xs) a)
deriving stock instance (Functor x, Functor (GenOp xs)) => Functor (GenOp (x ': xs))

type family OpListOf (genOp :: * -> *) :: [* -> *] where
    OpListOf (GenOp xs) = xs

type RemoveOp (op :: * -> *) (genOp :: * -> *)
    = GenOp (Remove (OpListOf genOp) (Found (OpListOf genOp) op))

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

class FoundOp (idx :: Nat) (op :: * -> *) (genOp :: * -> *) where
    liftOpRaw :: Proxy idx -> op a -> genOp a
    checkOpRaw :: Proxy idx -> genOp a -> Either (RemoveOp op genOp a) (op a)
    {-# MINIMAL liftOpRaw, checkOpRaw #-}

type ProxyFoundOp op genOp = Proxy (Found (OpListOf genOp) op)

-- | Lift a whole expression with the help of 'HasOp'.
liftExpression :: forall a b v . (Functor a, HasOp a b) => Expression a v -> Expression b v
liftExpression (Atom x) = Atom x
liftExpression (Op m) = Op $ liftOpRaw (Proxy :: ProxyFoundOp a b) $ fmap liftExpression m

-- | Indicate that @genOp@ includes operator @op@.
type HasOp op genOp = FoundOp (Found (OpListOf genOp) op) op genOp

-- | Lift an operator @op@ to @genOp@, where 'HasOp' @op@ @genOp@ holds.
liftOp :: forall op genOp a . HasOp op genOp => op a -> genOp a
liftOp = liftOpRaw (Proxy :: ProxyFoundOp op genOp)

-- | Checks whether a generalized operator @genOp@ is a @op@,
-- where 'HasOp' @op@ @genOp@ holds.
checkOp :: forall op genOp a . HasOp op genOp => genOp a -> Either (RemoveOp op genOp a) (op a)
checkOp = checkOpRaw (Proxy :: ProxyFoundOp op genOp)

-- | Shortcut constraint for 'HasOp'.
type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- | a is in (a : _)
instance FoundOp 'Z a (GenOp (a ': xs)) where
    liftOpRaw _ = HeadOp
    checkOpRaw _ (HeadOp m) = Right m
    checkOpRaw _ (TailOps ms) = Left ms

-- | a is in xs => a is in (x : xs)
instance FoundOp n a (GenOp xs) => FoundOp ('S n) a (GenOp (x ': xs)) where
    liftOpRaw _ = TailOps . liftOpRaw (Proxy :: Proxy n)
    checkOpRaw _ (TailOps ms) = case checkOpRaw (Proxy :: Proxy n) ms of
        Left l -> unsafeCoerce (Left (TailOps l))
        Right r -> Right r
    checkOpRaw _ (HeadOp m) = unsafeCoerce (Left (HeadOp m))

-- -- | [] subsets []
-- instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp '[]) where
--     liftOpRaw  = id
--     checkOpRaw = Just

-- -- | [] subsets (_ : _)
-- instance {-# OVERLAPPING #-} HasOp (GenOp '[]) (GenOp xs)
--     => HasOp (GenOp '[]) (GenOp (x ': xs)) where
--     liftOpRaw = TailOps . liftOpRaw
--     checkOpRaw (HeadOp _) = Nothing
--     checkOpRaw (TailOps m) = checkOpRaw m

-- -- | x is in g, xs is in g => (x : xs) is in g
-- instance {-# OVERLAPPING #-}
--     (HasOp x (GenOp (y ': ys)), HasOp (GenOp xs) (GenOp (y ': ys)))
--     => HasOp (GenOp (x ': xs)) (GenOp (y ': ys)) where
--     liftOpRaw (HeadOp m)   = liftOpRaw m
--     liftOpRaw (TailOps ms) = liftOpRaw ms
--     checkOpRaw g = HeadOp <$> checkOpRaw @x g
--               <|> TailOps <$> checkOpRaw @(GenOp xs) g