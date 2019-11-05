{-# LANGUAGE PolyKinds #-}
module Data.Expression.Utility where

-- | NonEmpty list type, intended for type level lists.
data NonEmpty a
    -- | Last item for the non-empty list.
    = Last a
    -- | Head and a 'NonEmpty' tail.
    | Cons a (NonEmpty a)

-- | Convert a non-empty list into a 'NonEmpty' list.
type family MkNonEmpty (lst :: [a]) :: NonEmpty a where
    MkNonEmpty '[x] = 'Last x
    MkNonEmpty (x ': xs) = 'Cons x (MkNonEmpty xs)