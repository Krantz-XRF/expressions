{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op where

import Data.Expression.Op.TH
import Data.Expression.GenOp

makeOp "MonoidOp" ''Num [("Plus", 2, '(+))]
makeOp "GroupOpDiff" ''Num [("Minus", 2, '(-)), ("Negate", 1, 'negate)]
makeOp "RingOpDiff" ''Num [("Multiply", 2, '(*))]
makeOp "FieldOpDiff" ''Fractional [("Recip", 1, 'recip), ("Divide", 2, '(/))]

type GroupOp = GenOp '[MonoidOp, GroupOpDiff]
type RingOp = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff]
type FieldOp = GenOp '[MonoidOp, GroupOpDiff, RingOpDiff, FieldOpDiff]