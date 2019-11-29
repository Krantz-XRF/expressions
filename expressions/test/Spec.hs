module Main where

import Test.Hspec

import Data.Expression

main :: IO ()
main = hspec spec

type EMonoid = Expression MonoidOp Int
type EGroup = Expression GroupOp Int
type ERing = Expression RingOp Int
type EField = Expression FieldOp Int

spec :: Spec
spec = describe "Expression Lifting & Checking" $ do
    it "Lift an Expr with MonoidOp to FieldOp" $
        liftExpression (OPlus (Atom 1) (Atom 2) :: EMonoid)
        `shouldBe` (OPlus (Atom 1) (Atom 2) :: EMonoid)
    it "Unlift an Expr from FieldOp to GroupOp" $
        checkOp (Minus (Atom 4) (Atom (3 :: Int)) :: FieldOp EField)
        `shouldBe` Right (Minus (Atom 4) (Atom 3) :: GroupOpDiff EField)
    it "Cannot unlift to some other Op" $
        checkOp @MonoidOp (Minus (Atom 4) (Atom (3 :: Int)) :: FieldOp EField)
        `shouldBe` Left (Minus (Atom 4) (Atom 3))