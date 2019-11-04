{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op where

import Data.Expression.Op.TH
import Data.Expression.GenOp

-- * Monoid operator
makeOp "MonoidOp"    [("Plus", '(+))]

-- * Group operator
makeOp "GroupOpDiff" [("Minus", '(-)), ("Negate", 'negate)]
type GroupOp = GenOp '[MonoidOp, GroupOpDiff]

-- * Ring operator
makeOp "RingOpDiff"  [("Multiply", '(*))]
type RingOp  = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff]

-- * Field operator
makeOp "FieldOpDiff" [("Recip", 'recip), ("Divide", '(/))]
type FieldOp = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff, FieldOpDiff]