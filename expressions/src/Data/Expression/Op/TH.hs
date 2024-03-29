{-# LANGUAGE TemplateHaskell #-}
module Data.Expression.Op.TH (
      -- * Automatic Op Generating
      --
      -- $groupDecl
      -- $opSynonyms
      -- $exprSynonyms
      -- $evalOpInstance
      makeOp
    ) where

import Data.Char
import qualified Data.Set as Set
import Control.Monad

import Language.Haskell.TH

import Data.Expression.Basic
import Data.Expression.GenOp

import Data.Functor.Identity

pattern MkArrow :: Type -> Type -> Type
pattern MkArrow t1 t2 = ArrowT `AppT` t1 `AppT` t2

-- $groupDecl
-- Here is what will be generated by a 'makeOp' call.
--
-- > makeOp "<groupName>" [(<ops:name>, <ops:impl>)]
--
-- > data <groupName> a
-- >   = <ops:name> a ... <ops:cnt> | ...
-- >   deriving stock (Show, Eq, Functor)
makeOpGroupDecl :: String -> [(String, Int)] -> Q [Dec]
makeOpGroupDecl groupName ops = do
    let noBang = Bang NoSourceUnpackedness NoSourceStrictness
    a <- newName "a"
    return $ pure $ DataD [] (mkName groupName) [KindedTV a StarT] Nothing
        [NormalC nm' $ replicate cnt (noBang, VarT a)
            | (nm, cnt) <- ops, let nm' = mkName (head groupName : nm)]
        [DerivClause (Just StockStrategy) (ConT <$> [''Show, ''Eq, ''Functor])]

-- $exprSynonyms
-- Generalized Expression Synonyms (via 'HasOp'):
--
-- > let: <gName> = <head groupName><ops:name>
-- > pattern <ops:name>
-- >   :: HasOp <groupName> op => a -> ... <ops:cnt> -> op a
-- > pattern <ops:name> x ... <ops:cnt>
-- >   <- (checkOp -> Right (<gName> x ... <ops:cnt>))
-- >   where <ops:name> x y = liftOp (<gName> x y)
makeExprSynonym :: String -> [(String, Int)] -> Q [Dec]
makeExprSynonym groupName ops = fmap concat $ forM ops $ \(nm, cnt) -> do
    varOp <- newName "op"
    varA <- newName "a"
    let opSynName = mkName nm
    let opGroupName = mkName (head groupName : nm)
    let result = VarT varOp `AppT` VarT varA
    vars <- replicateM cnt (newName "x")
    return
        [ PatSynSigD opSynName $
            ForallT [KindedTV varA StarT, KindedTV varOp (StarT `MkArrow` StarT)]
                [ConT ''HasOp `AppT` ConT (mkName groupName) `AppT` VarT varOp]
                (foldr ($) result $ replicate cnt (MkArrow (VarT varA)))
        , PatSynD opSynName (PrefixPatSyn vars)
            (ExplBidir [Clause (VarP <$> vars)
                (NormalB $ VarE 'liftOp `AppE`
                    foldl AppE (ConE opGroupName) (map VarE vars)) []])
            (ViewP (VarE 'checkOp) (ConP 'Right [ConP opGroupName (VarP <$> vars)]))
        ]

-- $opSynonyms
-- Generalized Operator Synonyms (via 'HasOp'):
--
-- > pattern O<ops:name>
-- >   :: HasOp <groupName> op
-- >   => Expression op a
-- >   -> ... <ops:cnt>
-- >   -> Expression op a
-- > pattern O<ops:name> x ... <ops:cnt>
-- >   = Op (<ops:name> x ... <ops:cnt>)
makeOpSynonym :: String -> [(String, Int)] -> Q [Dec]
makeOpSynonym groupName ops = fmap concat $ forM ops $ \(nm, cnt) -> do
    varOp <- newName "op"
    varA <- newName "a"
    let exprConName = mkName ('O' : nm)
    let opSynName = mkName nm
    let expr = ConT ''Expression `AppT` VarT varOp `AppT` VarT varA
    vars <- replicateM cnt (newName "x")
    return 
        [ PatSynSigD exprConName $
            ForallT [KindedTV varA StarT, KindedTV varOp (StarT `MkArrow` StarT)]
                [ConT ''HasOp `AppT` ConT (mkName groupName) `AppT` VarT varOp]
                (foldr ($) expr $ replicate cnt (MkArrow expr))
        , PatSynD exprConName (PrefixPatSyn vars) ImplBidir
            (ConP 'Op [ConP opSynName (VarP <$> vars)])
        ]

-- $evalOpInstance
-- 'EvalOpM' Instance for 'Identity' monad:
--
-- > let: <gName> = <head groupName><ops:name>
-- > instance EvalOpM Identity <groupName> where
-- >   evalOp (<gName> x ... <ops:cnt>)
-- >     = return (<ops:impl> (eval x) ... <ops:cnt>)
-- >   ... <length ops>
makeEvalOpInstance :: String -> Name -> Type -> [(String, Int, Name)] -> Q [Dec]
makeEvalOpInstance groupName varA ctxt ops = do
    let gName = mkName groupName
    let typeAlias = TySynInstD ''CanEvalM (TySynEqn [ConT ''Identity, ConT gName, VarT varA] ctxt)
    funClauses <- forM ops $ \(nm, cnt, impl) -> do
        vars <- replicateM cnt (newName "x")
        return $ Clause [ConP (mkName $ head groupName : nm) (VarP <$> vars)]
            (NormalB $ AppE (VarE 'return)
                     $ foldl AppE (VarE impl)
                     $ AppE (VarE 'eval) <$> VarE <$> vars) []
    return $ pure $ InstanceD Nothing [] (ConT ''EvalOpM `AppT` ConT ''Identity `AppT` ConT gName)
        [typeAlias, FunD 'evalOp funClauses]

fetchConstraintFor :: Name -> Name -> Q (Cxt, Int)
fetchConstraintFor var name = reify name >>= \case
    VarI _ typ _ -> process [] [] typ
    ClassOpI _ typ _ -> process [] [] typ
    _ -> fail "Invalid opImpl: Not supported."
    where
    -- only allow v or (v :: *)
    viewVarBndr :: TyVarBndr -> Maybe Name
    viewVarBndr (PlainTV v) = Just v
    viewVarBndr (KindedTV v StarT) = Just v
    viewVarBndr _ = Nothing
    -- only allow forall x . C x => x -> ... -> x
    process :: [Name] -> Cxt -> Type -> Q (Cxt, Int)
    process vars ctxt (ForallT [viewVarBndr -> Just v] newCtxt typ)
        = process (v:vars) (newCtxt ++ ctxt) typ
    process vars ctxt typ = case vars of
        (v:vs) | all (v ==) vs -> processSimple v ctxt 0 typ
        _ -> fail "Only 1 variable should be used."
    -- the top level forall's are removed
    -- so here only allow x -> ... -> x
    processSimple :: Name -> Cxt -> Int -> Type -> Q (Cxt, Int)
    processSimple v ctxt cnt (MkArrow (VarT x) typ)
        | x == v = processSimple v ctxt (succ cnt) typ
    processSimple v ctxt cnt (VarT x)
        | x == v = return (replaceVar v ctxt, cnt)
    processSimple _ _ _ _ = fail "Only type a^n -> a is allowed."
    -- make (C x) becomes (C var)
    replaceVar :: Name -> Cxt -> Cxt
    replaceVar v = map replaceOnce where
        replaceOnce :: Type -> Type
        replaceOnce (AppT f x) = AppT (replaceOnce f) (replaceOnce x)
        replaceOnce (SigT x k) = SigT (replaceOnce x) k
        replaceOnce (ParensT x) = ParensT (replaceOnce x)
        replaceOnce (VarT x) | x == v = VarT var
        replaceOnce t = t

-- | Make operators from @groupName@ and (@opName@, @opImpl@) pairs.
makeOp :: String -> [(String, Name)] -> Q [Dec]
makeOp groupName ops = do
    when (null groupName || any (null . fst) ops) $
        fail "Operator should not be empty."
    unless (all (isUpper . head) (groupName : map fst ops)) $
        fail "Names should begin with uppercase letters."
    let (opNames, opImpls) = unzip ops
    varA <- newName "a"
    (opCxt, opCnt) <- unzip <$> mapM (fetchConstraintFor varA) opImpls
    let opNamesCnts = zip opNames opCnt
    let ctxtTotal = Set.toList $ Set.fromList $ concat opCxt
    let ctxt = foldl AppT (TupleT $ length ctxtTotal) ctxtTotal
    groupDecl <- makeOpGroupDecl groupName opNamesCnts
    opSynonym <- makeOpSynonym groupName opNamesCnts
    exprSynonym <- makeExprSynonym groupName opNamesCnts
    evalOpInstance <- makeEvalOpInstance groupName varA ctxt (zip3 opNames opCnt opImpls)
    return $ groupDecl ++ opSynonym ++ exprSynonym ++ evalOpInstance