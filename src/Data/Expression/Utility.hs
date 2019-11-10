{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Expression.Utility where

-- | Inductive natural numbers.
data Nat = Z | S Nat deriving (Show, Eq, Ord)

-- | Get the @n@th element of type level list @ts@.
type family IndexOf (ts :: [k]) (n :: Nat) :: k where
    IndexOf (k ': _) 'Z = k
    IndexOf (_ ': ks) ('S n) = IndexOf ks n

-- | Get the index of a given element @t@ in a type level list @ts@.
type family Found (ts :: [k]) (t :: k) :: Nat where
    Found (t ': ts) t = 'Z
    Found (u ': ts) t = 'S (Found ts t)

-- | Remove an element at the given index @n@ from a type level list @ts@.
type family Remove (ts :: [k]) (n :: Nat) :: [k] where
    Remove (_ ': ks) 'Z = ks
    Remove (k ': ks) ('S n) = k ': Remove ks n

-- | Get the length of a type level list.
type family Length (ts :: [k]) :: Nat where
    Length '[] = 'Z
    Length (x ': xs) = 'S (Length xs)

-- | Subtract the list @xs@ from list @ys@.
type family ListDiff (xs :: [k]) (ys :: [k]) :: [k] where
    ListDiff xs '[] = xs
    ListDiff xs (y ': ys) = ListDiff (Remove xs (Found xs y)) ys