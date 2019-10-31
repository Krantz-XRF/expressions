{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op.TH (makeOp) where

import Data.Char
import Control.Monad

import Language.Haskell.TH

import Data.Expression.Basic
import Data.Expression.GenOp

pattern MkArrow :: Type -> Type -> Type
pattern MkArrow t1 t2 = ArrowT `AppT` t1 `AppT` t2

-- data <groupName> a
--   = <ops:name> a ... <ops:cnt> | ...
--   deriving stock (Show, Eq, Functor)
makeOpGroupDecl :: String -> [(String, Int)] -> Q [Dec]
makeOpGroupDecl groupName ops = do
    let noBang = Bang NoSourceUnpackedness NoSourceStrictness
    a <- newName "a"
    return $ pure $ DataD [] (mkName groupName) [KindedTV a StarT] Nothing
        [NormalC nm' $ replicate cnt (noBang, VarT a)
            | (nm, cnt) <- ops, let nm' = mkName (head groupName : nm)]
        [DerivClause (Just StockStrategy) (ConT <$> [''Show, ''Eq, ''Functor])]

-- pattern O<ops:name>
--   :: HasOp <groupName> op
--   => Expression op a
--   -> ... <ops:cnt>
--   -> Expression op a
-- pattern <ops:name>
--   :: HasOp <groupName> op
--   => Expression op a
--   -> ... <ops:cnt>
--   -> op (Expression op a)
makeSynonymSig :: String -> [(String, Int)] -> Q [Dec]
makeSynonymSig groupName ops = do
    varOp <- newName "op"
    varA <- newName "a"
    let expr = ConT ''Expression `AppT` VarT varOp `AppT` VarT varA
        exprRaw = VarT varOp `AppT` expr
        synonymSig result name cnt = PatSynSigD (mkName name) $
            ForallT [KindedTV varA StarT, KindedTV varOp (StarT `MkArrow` StarT)]
                [ConT ''HasOp `AppT` ConT (mkName groupName) `AppT` VarT varOp]
                (foldr ($) result $ replicate cnt (MkArrow expr))
    return $ map (uncurry $ synonymSig expr) ops
           ++ map (uncurry $ synonymSig exprRaw . ('O' : )) ops

-- let: <gName> = <groupName:head><ops:name>
-- pattern O<ops:name> x ... <ops:cnt>
--   <- (checkOp -> Just (<gName> x ... <ops:cnt>))
--   where O<ops:name> x y = liftOp (<gName> x y)
makeOpSynonym :: String -> [(String, Int)] -> Q [Dec]
makeOpSynonym groupName ops = forM ops $ \(nm, cnt) -> do
    let opSynName = mkName ('O' : nm)
    let opGroupName = mkName (head groupName : nm)
    vars <- replicateM cnt (newName "x")
    return $ PatSynD opSynName (PrefixPatSyn vars)
        (ExplBidir [Clause (VarP <$> vars)
            (NormalB $ VarE 'liftOp `AppE`
                foldl AppE (ConE opGroupName) (map VarE vars)) []])
        (ViewP (VarE 'checkOp) (ConP 'Just [ConP opGroupName (VarP <$> vars)]))

-- pattern <ops:name> x ... <ops:cnt>
--   = Op (O<ops:name> x ... <ops:cnt>)
makeExprSynonym :: [(String, Int)] -> Q [Dec]
makeExprSynonym ops = forM ops $ \(nm, cnt) -> do
    let exprConName = mkName nm
    let opSynName = mkName ('O' : nm)
    vars <- replicateM cnt (newName "x")
    return $ PatSynD exprConName (PrefixPatSyn vars) ImplBidir
        (ConP 'Op [ConP opSynName (VarP <$> vars)])

makeEvalOpInstance :: String -> Name -> [(String, Int, Name)] -> Q [Dec]
makeEvalOpInstance groupName ctxt ops = do
    let gName = mkName groupName
    varA <- newName "a"
    let typeAlias = TySynInstD ''CanEval
            $ TySynEqn [ConT gName, VarT varA] (AppT (ConT ctxt) (VarT varA))
    funClauses <- forM ops $ \(nm, cnt, impl) -> do
        vars <- replicateM cnt (newName "x")
        return $ Clause [ConP (mkName $ head groupName : nm) (VarP <$> vars)]
            (NormalB $ foldl AppE (VarE impl) $ AppE (VarE 'eval) <$> VarE <$> vars) []
    return $ pure $ InstanceD Nothing [] (ConT ''EvalOp `AppT` ConT gName)
        [typeAlias, FunD 'evalOp funClauses]

makeOp :: String -> Name -> [(String, Int, Name)] -> Q [Dec]
makeOp groupName ctxt ops = do
    let getName (nm, _, _) = nm
    when (null groupName || any (null . getName) ops) $
        reportError "Operator should not be empty."
    unless (all (isUpper . head) (groupName : map getName ops)) $
        reportError "Names should begin with uppercase letters."
    let ops2 = map (\(nm, cnt, _) -> (nm, cnt)) ops
    groupDecl <- makeOpGroupDecl groupName ops2
    synonymSig <- makeSynonymSig groupName ops2
    opSynonym <- makeOpSynonym groupName ops2
    exprSynonym <- makeExprSynonym ops2
    evalOpInstance <- makeEvalOpInstance groupName ctxt ops
    return $ groupDecl ++ synonymSig ++ opSynonym ++ exprSynonym ++ evalOpInstance