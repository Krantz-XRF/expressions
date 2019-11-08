{-# LANGUAGE PolyKinds #-}
module Data.Expression.Utility where

type family Concat (xs :: [a]) (ys :: [a]) :: [a] where
    Concat '[] ys = ys
    Concat (x ': xs) ys = x ': Concat xs ys