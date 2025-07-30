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

deriveTr :: Env -> Exp -> Maybe Derivation
deriveTr env (I i) = pure $ Derivation (Tr env (I i) (I' i)) "Tr-Int" []
deriveTr env (B b) = pure $ Derivation (Tr env (B b) (B' b)) "Tr-Bool" []
deriveTr (Snoc env y) (V x)
    | x == y = pure $ Derivation (Tr (Snoc env y) (V x) (V' 1)) "Tr-Var1" []
    | Just p <- deriveTr env (V x)
    , (Tr _ _ (V' i)) <- conclusion p
    = pure $ Derivation (Tr (Snoc env y) (V x) (V' (i + 1))) "Tr-Var2" [p]
deriveTr env (Add e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    = pure $ Derivation (Tr env (Add e1 e2) (Add' e1' e2')) "Tr-Plus" [p1, p2]
deriveTr env (Sub e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    = pure $ Derivation (Tr env (Sub e1 e2) (Sub' e1' e2')) "Tr-Minus" [p1, p2]
deriveTr env (Mul e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    = pure $ Derivation (Tr env (Mul e1 e2) (Mul' e1' e2')) "Tr-Times" [p1, p2]
deriveTr env (Less e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    = Just $ Derivation (Tr env (Less e1 e2) (Less' e1' e2')) "Tr-Lt" [p1, p2]
deriveTr env (Ite e1 e2 e3)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    , Just p3 <- deriveTr env e3
    , (Tr _ _ e3') <- conclusion p3
    = Just $ Derivation (Tr env (Ite e1 e2 e3) (Ite' e1' e2' e3')) "Tr-If" [p1, p2, p3]
deriveTr env (Let x e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr (Snoc env x) e2
    , (Tr _ _ e2') <- conclusion p2
    = Just $ Derivation (Tr env (Let x e1 e2) (Let' e1' e2')) "Tr-Let" [p1, p2]
deriveTr env (Fun x e)
    | Just p <- deriveTr (Snoc env x) e
    , (Tr _ _ e') <- conclusion p
    = Just $ Derivation (Tr env (Fun x e) (Fun' e')) "Tr-Fun" [p]
deriveTr env (App e1 e2)
    | Just p1 <- deriveTr env e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr env e2
    , (Tr _ _ e2') <- conclusion p2
    = Just $ Derivation (Tr env (App e1 e2) (App' e1' e2')) "Tr-App" [p1, p2]
deriveTr env (Letrec f x e1 e2)
    | Just p1 <- deriveTr (Snoc (Snoc env f) x) e1
    , (Tr _ _ e1') <- conclusion p1
    , Just p2 <- deriveTr (Snoc env f) e2
    , (Tr _ _ e2') <- conclusion p2
    = Just $ Derivation (Tr env (Letrec f x e1 e2) (Letrec' e1' e2')) "Tr-LetRec" [p1, p2]
deriveTr _ _ = Nothing

main :: IO ()
main = do
    s <- getContents
    let (env, e) = parse (alexScanTokens s)
    let derivationTree = deriveTr env e
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> print d
