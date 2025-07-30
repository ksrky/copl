module EvalNatExp where

import           Data.List (intercalate)

import           Lexer
import           Parser
import           Syntax

data Derivation = Derivation {
    conclusion :: Judgement,
    ruleName   :: String,
    premises   :: [Derivation]
}

instance Show Derivation where
    show = prettyPrint 0
      where
        prettyPrint :: Int -> Derivation -> String
        prettyPrint indent d =
            let spaces = replicate (indent * 2) ' '
                conclusionStr = show (conclusion d) ++ " by " ++ ruleName d
            in case premises d of
                [] -> spaces ++ conclusionStr ++ " {}"
                ps -> let premiseStrings = map (prettyPrint (indent + 1)) ps
                    in spaces ++ conclusionStr ++ " {\n" ++
                        intercalate ";\n" premiseStrings ++ "\n" ++
                        spaces ++ "}"

derivePlus :: Int -> Int -> Derivation
derivePlus i1 i2 = Derivation (Plus i1 i2 (i1 + i2)) "B-Plus" []

deriveMinus :: Int -> Int -> Derivation
deriveMinus i1 i2 = Derivation (Minus i1 i2 (i1 - i2)) "B-Minus" []

deriveTimes :: Int -> Int -> Derivation
deriveTimes i1 i2 = Derivation (Times i1 i2 (i1 * i2)) "B-Times" []

deriveLess :: Int -> Int -> Derivation
deriveLess i1 i2 = Derivation (Lt i1 i2 (i1 < i2)) "B-Lt" []

find :: Env -> String -> Maybe Val
find Empty _ = Nothing
find (Snoc env x v) y
    | x == y = Just v
    | otherwise = find env y

compose :: Env -> Env -> Env
compose env1 Empty           = env1
compose env1 (Snoc env2 x v) = Snoc (compose env1 env2) x v

deriveEvalTo :: Env -> Exp -> Maybe Derivation
deriveEvalTo env (I i) = pure $ Derivation (EvalTo env (I i) (VInt i)) "E-Int" []
deriveEvalTo env (B b) = pure $ Derivation (EvalTo env (B b) (VBool b)) "E-Bool" []
deriveEvalTo env (V x)
    | Just v <- find env x = pure $ Derivation (EvalTo env (V x) v) "E-Var" []
deriveEvalTo env (Add e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ (VInt i2)) <- conclusion p2
    , let p3 = derivePlus i1 i2
    , (Plus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo env (Add e1 e2) (VInt i3)) "E-Plus" [p1, p2, p3]
deriveEvalTo env (Sub e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ (VInt i2)) <- conclusion p2
    , let p3 = deriveMinus i1 i2
    , (Minus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo env (Sub e1 e2) (VInt i3)) "E-Minus" [p1, p2, p3]
deriveEvalTo env (Mul e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ (VInt i2)) <- conclusion p2
    , let p3 = deriveTimes i1 i2
    , (Times _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo env (Mul e1 e2) (VInt i3)) "E-Times" [p1, p2, p3]
deriveEvalTo env (Less e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ (VInt i2)) <- conclusion p2
    , let p3 = deriveLess i1 i2
    , (Lt _ _ b) <- conclusion p3
    = pure $ Derivation (EvalTo env (Less e1 e2) (VBool b)) "E-Lt" [p1, p2, p3]
deriveEvalTo env (Ite e1 e2 e3)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VBool True)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Ite e1 e2 e3) v2) "E-IfT" [p1, p2]
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VBool False)) <- conclusion p1
    , Just p3 <- deriveEvalTo env e3
    , (EvalTo _ _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo env (Ite e1 e2 e3) v3) "E-IfF" [p1, p3]
deriveEvalTo env (Let x e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ v1) <- conclusion p1
    , let env' = Snoc env x v1
    , Just p2 <- deriveEvalTo env' e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Let x e1 e2) v2) "E-Let" [p1, p2]
deriveEvalTo env (Fun x e) =
    Just $ Derivation (EvalTo env (Fun x e) (VClos env x e)) "E-Fun" []
deriveEvalTo env (App e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VFix env0 f x e0)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    , let env' = Snoc (Snoc env0 f (VFix env0 f x e0)) x v2
    , Just p3 <- deriveEvalTo env' e0
    , (EvalTo _ _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo env (App e1 e2) v3) "E-AppRec" [p1, p2, p3]
deriveEvalTo env (App e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VClos env0 x e)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    , let env' = Snoc env0 x v2
    , Just p3 <- deriveEvalTo env' e
    , (EvalTo _ _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo env (App e1 e2) v3) "E-App" [p1, p2, p3]
deriveEvalTo env (Letrec f x e1 e2)
    | let env' = Snoc env f (VFix env f x e1)
    , Just p2 <- deriveEvalTo env' e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Letrec f x e1 e2) v2) "E-LetRec" [p2]
deriveEvalTo env Nil = Just $ Derivation (EvalTo env Nil VNil) "E-Nil" []
deriveEvalTo env (Cons e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ v1) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Cons e1 e2) (VCons v1 v2)) "E-Cons" [p1, p2]
deriveEvalTo env (Match e (Clause p e1))
    | Just d1 <- deriveEvalTo env e
    , (EvalTo _ _ v) <- conclusion d1
    , Just d2 <- deriveMatch p v
    , (Matches _ _ env1) <- conclusion d2
    , let env' = compose env env1
    , Just d3 <- deriveEvalTo env' e1
    , (EvalTo _ _ v1) <- conclusion d3
    = Just $ Derivation (EvalTo env (Match e (Clause p e1)) v1) "E-MatchM1" [d1, d2, d3]
deriveEvalTo env (Match e (Clauses p e1 cls))
    | Just d1 <- deriveEvalTo env e
    , (EvalTo _ _ v) <- conclusion d1
    , Just d2 <- deriveMatch p v
    , (Matches _ _ env1) <- conclusion d2
    , let env' = compose env env1
    , Just d3 <- deriveEvalTo env' e1
    , (EvalTo _ _ v1) <- conclusion d3
    = Just $ Derivation (EvalTo env (Match e (Clauses p e1 cls)) v1) "E-MatchM2" [d1, d2, d3]
deriveEvalTo env (Match e (Clauses p e1 cls))
    | Just d1 <- deriveEvalTo env e
    , (EvalTo _ _ v) <- conclusion d1
    , Just d2 <- deriveMatch p v
    , MatchFail _ _ <- conclusion d2
    , Just d3 <- deriveEvalTo env (Match e cls)
    , (EvalTo _ _ v1) <- conclusion d3
    = Just $ Derivation (EvalTo env (Match e (Clauses p e1 cls)) v1) "E-MatchN" [d1, d2, d3]
deriveEvalTo _ _ = Nothing

deriveMatch :: Pat -> Val -> Maybe Derivation
deriveMatch (PVar x) v
    = Just $ Derivation (Matches (PVar x) v (Snoc Empty x v)) "M-Var" []
deriveMatch PNil VNil
    = Just $ Derivation (Matches PNil VNil Empty) "M-Nil" []
deriveMatch (PCons p1 p2) (VCons v1 v2)
    | Just d1 <- deriveMatch p1 v1
    , (Matches _ _ env1) <- conclusion d1
    , Just d2 <- deriveMatch p2 v2
    , (Matches _ _ env2) <- conclusion d2
    = Just $ Derivation (Matches (PCons p1 p2) (VCons v1 v2) (compose env1 env2)) "M-Cons" [d1, d2]
    | Just d <- deriveMatch p1 v1
    , MatchFail _ _ <- conclusion d
    = Just $ Derivation (MatchFail (PCons p1 p2) (VCons v1 v2)) "NM-ConsConsL" [d]
    | Just d <- deriveMatch p2 v2
    , MatchFail _ _ <- conclusion d
    = Just $ Derivation (MatchFail (PCons p1 p2) (VCons v1 v2)) "NM-ConsConsR" [d]
deriveMatch PWild v = Just $ Derivation (Matches PWild v Empty) "M-Wild" []
deriveMatch p@PCons{} VNil = Just $ Derivation (MatchFail p VNil) "NM-NilCons" []
deriveMatch PNil v@VCons{} = Just $ Derivation (MatchFail PNil v) "NM-ConsNil" []
deriveMatch _ _ = Nothing

main :: IO ()
main = do
    s <- getContents
    let (env, e) = parse (alexScanTokens s)
    let derivationTree = deriveEvalTo env e
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> print d
