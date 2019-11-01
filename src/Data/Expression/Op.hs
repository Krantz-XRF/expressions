{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op where

import Data.Expression.Op.TH
import Data.Expression.GenOp

makeOp "MonoidOp"    [("Plus", '(+))]
makeOp "GroupOpDiff" [("Minus", '(-)), ("Negate", 'negate)]
makeOp "RingOpDiff"  [("Multiply", '(*))]
makeOp "FieldOpDiff" [("Recip", 'recip), ("Divide", '(/))]

type GroupOp = GenOp '[MonoidOp, GroupOpDiff]
type RingOp  = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff]
type FieldOp = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff, FieldOpDiff]