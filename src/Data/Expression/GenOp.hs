{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Expression.GenOp
    ( GenOp(..)
    , GenExpression
    , HasOp
    , liftOp
    , checkOp
    , HasOps
    , liftExpression
    , replaceOp
    ) where

import GHC.Exts (Constraint)

import GHC.Exts (Proxy#, proxy#)

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

type RemoveOp (op :: * -> *) (genOp :: * -> *) = GenOp (RemoveList op (OpListOf genOp))

type family RemoveList (op :: * -> *) (ops :: [* -> *]) :: [* -> *] where
    RemoveList (GenOp xs) ys = ListDiff ys xs
    RemoveList x ys = Remove ys (Found ys x)

type family IsGenOp (op :: * -> *) :: Bool where
    IsGenOp (GenOp _) = 'True
    IsGenOp _         = 'False

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

class FoundOp (subset :: Bool) (n :: Nat) (op :: * -> *) (genOp :: * -> *) where
    liftOpRaw :: Proxy# subset -> Proxy# n -> op a -> genOp a
    checkOpRaw :: Proxy# subset -> Proxy# n -> genOp a -> Either (RemoveOp op genOp a) (op a)
    {-# MINIMAL liftOpRaw, checkOpRaw #-}

type family IndexOrLength (genOp :: * -> *) (op :: * -> *) :: Nat where
    IndexOrLength (GenOp _) (GenOp xs) = Length xs
    IndexOrLength (GenOp xs) x = Found xs x

type PIndexOrLength op genOp = Proxy# (IndexOrLength genOp op)
type ProxyIsGenOp op = Proxy# (IsGenOp op)

-- | Lift a whole expression with the help of 'HasOp'.
liftExpression :: forall a b v . (Functor a, HasOp a b) => Expression a v -> Expression b v
liftExpression (Atom x) = Atom x
liftExpression (Op m) = Op $ liftOp $ fmap liftExpression m

-- | Replace all occurrences of a operator in the expression tree.
replaceOp :: ( Functor x, Functor (GenOp (RemoveList x (OpListOf g)))
             , HasOp x g, HasOp (RemoveOp x g) h)
          => ((Expression g a -> Expression h a) -> x (Expression g a) -> Expression h a)
          -> Expression g a
          -> Expression h a
replaceOp f (Op m) = case checkOp m of
    Right l -> f (replaceOp f) l
    Left r -> Op $ liftOp $ fmap (replaceOp f) r
replaceOp _ (Atom a) = Atom a

-- | Indicate that @genOp@ includes operator @op@.
type HasOp op genOp = FoundOp (IsGenOp op) (IndexOrLength genOp op) op genOp

-- | Lift an operator @op@ to @genOp@, where 'HasOp' @op@ @genOp@ holds.
liftOp :: forall op genOp a . HasOp op genOp => op a -> genOp a
liftOp = liftOpRaw (proxy# :: ProxyIsGenOp op) (proxy# :: PIndexOrLength op genOp)

-- | Checks whether a generalized operator @genOp@ is a @op@,
-- where 'HasOp' @op@ @genOp@ holds.
checkOp :: forall op genOp a . HasOp op genOp => genOp a -> Either (RemoveOp op genOp a) (op a)
checkOp = checkOpRaw (proxy# :: ProxyIsGenOp op) (proxy# :: PIndexOrLength op genOp)

-- | Shortcut constraint for 'HasOp'.
type family HasOps (ops :: [* -> *]) (genOp :: * -> *) :: Constraint where
    HasOps '[]       genOp = ()
    HasOps (x ': xs) genOp = (HasOp x genOp, HasOps xs genOp)

-- a is in (a : _)
instance RemoveOp a (GenOp (a : xs)) ~ GenOp xs
    => FoundOp 'False 'Z a (GenOp (a ': xs)) where
    liftOpRaw _ _ = HeadOp
    checkOpRaw _ _ (HeadOp m) = Right m
    checkOpRaw _ _ (TailOps ms) = Left ms

-- a is in xs => a is in (x : xs)
instance
    ( RemoveList a (x : xs) ~ (x : RemoveList a xs)
    , FoundOp 'False n a (GenOp xs))
    => FoundOp 'False ('S n) a (GenOp (x ': xs)) where
    liftOpRaw p _ = TailOps . liftOpRaw p (proxy# :: Proxy# n)
    checkOpRaw p _ (TailOps ms) = case checkOpRaw p (proxy# :: Proxy# n) ms of
        Left l -> Left (TailOps l)
        Right r -> Right r
    checkOpRaw _ _ (HeadOp m) = Left (HeadOp m)

-- [] subsets []
instance FoundOp 'True 'Z (GenOp '[]) (GenOp '[]) where
    liftOpRaw  _ _ = id
    checkOpRaw _ _ = Right

-- [] subsets (_ : _)
instance FoundOp 'True 'Z (GenOp '[]) (GenOp xs)
    => FoundOp 'True 'Z (GenOp '[]) (GenOp (x ': xs)) where
    liftOpRaw pt pz = TailOps . liftOpRaw pt pz
    checkOpRaw pt pz (TailOps m) = case checkOpRaw pt pz m of
        Left l -> Left (TailOps l)
        Right r -> Right r
    checkOpRaw _ _ m = Left m

-- x is in g, xs is in g => (x : xs) is in g
instance
    ( ListDiff (RemoveList x (y : ys)) xs
      ~ ListDiff (Remove (y : ys) (Found (y : ys) x)) xs
    , HasOp x (GenOp (y ': ys))
    , FoundOp 'True n (GenOp xs) (GenOp (y ': ys))
    , FoundOp 'True n (GenOp xs) (RemoveOp x (GenOp (y ': ys))))
    => FoundOp 'True ('S n) (GenOp (x ': xs)) (GenOp (y ': ys)) where
    liftOpRaw _ _ (HeadOp m)   = liftOp m
    liftOpRaw pt _ (TailOps ms) = liftOpRaw pt (proxy# :: Proxy# n) ms
    checkOpRaw pt _ g = case checkOp g of
        Right r -> Right (HeadOp r)
        Left l -> case checkOpRaw pt (proxy# :: Proxy# n) l of
            Right r -> Right (TailOps r)
            Left l' -> Left l'