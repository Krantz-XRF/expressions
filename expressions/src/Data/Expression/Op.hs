{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op where

import Data.Expression.Op.TH
import Data.Expression.GenOp

-- * Monoid operator
makeOp "MonoidOp"    [("Plus", '(+))]

-- * Group operator
makeOp "GroupOpDiff" [("Minus", '(-)), ("Negate", 'negate)]
-- | Operators for an (algebraic) addition group:
-- x+y, x-y, -x
type GroupOp = GenOp '[MonoidOp, GroupOpDiff]

-- * Ring operator
makeOp "RingOpDiff"  [("Multiply", '(*))]
-- | Operators for an (algebraic) ring:
-- x+y, x-y, -x, x*y
type RingOp  = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff]

-- * Field operator
makeOp "FieldOpDiff" [("Recip", 'recip), ("Divide", '(/))]
-- | Operators for an (algebraic) field:
-- x+y, x-y, -x, x*y, x\/y, 1\/x
type FieldOp = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff, FieldOpDiff]