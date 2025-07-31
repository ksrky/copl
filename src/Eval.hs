module Eval where

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

nth :: Env -> Idx -> Maybe Val
nth Empty _        = Nothing
nth (Snoc _ v) 1   = Just v
nth (Snoc env _) n = nth env (n - 1)

deriveEvalTo :: Env -> Exp -> Maybe Derivation
deriveEvalTo env (I i) = pure $ Derivation (EvalTo env (I i) (VInt i)) "E-Int" []
deriveEvalTo env (B b) = pure $ Derivation (EvalTo env (B b) (VBool b)) "E-Bool" []
deriveEvalTo env (V i)
    | Just v <- nth env i
    = pure $ Derivation (EvalTo env (V i) v) "E-Var" []
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
deriveEvalTo env (Let e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ v1) <- conclusion p1
    , let env' = Snoc env v1
    , Just p2 <- deriveEvalTo env' e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Let e1 e2) v2) "E-Let" [p1, p2]
deriveEvalTo env (Fun e) =
    Just $ Derivation (EvalTo env (Fun e) (VClos env e)) "E-Fun" []
deriveEvalTo env (App e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VFix env0 e0)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    , let env' = Snoc (Snoc env0 (VFix env0 e0)) v2
    , Just p3 <- deriveEvalTo env' e0
    , (EvalTo _ _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo env (App e1 e2) v3) "E-AppRec" [p1, p2, p3]
deriveEvalTo env (App e1 e2)
    | Just p1 <- deriveEvalTo env e1
    , (EvalTo _ _ (VClos env0 e)) <- conclusion p1
    , Just p2 <- deriveEvalTo env e2
    , (EvalTo _ _ v2) <- conclusion p2
    , let env' = Snoc env0 v2
    , Just p3 <- deriveEvalTo env' e
    , (EvalTo _ _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo env (App e1 e2) v3) "E-App" [p1, p2, p3]
deriveEvalTo env (Letrec e1 e2)
    | let env' = Snoc env (VFix env e1)
    , Just p2 <- deriveEvalTo env' e2
    , (EvalTo _ _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo env (Letrec e1 e2) v2) "E-LetRec" [p2]
deriveEvalTo _ _ = Nothing

main :: IO ()
main = do
    s <- getContents
    let (env, e) = parse (alexScanTokens s)
    let derivationTree = deriveEvalTo env e
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> print d
