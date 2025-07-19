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

deriveEvalTo :: Exp -> Maybe Derivation
deriveEvalTo (I i) = pure $ Derivation (EvalTo (I i) (VInt i)) "E-Int" []
deriveEvalTo (B b) = pure $ Derivation (EvalTo (B b) (VBool b)) "E-Bool" []
deriveEvalTo (Add e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (VInt i2)) <- conclusion p2
    , let p3 = derivePlus i1 i2
    , (Plus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Add e1 e2) (VInt i3)) "E-Plus" [p1, p2, p3]
deriveEvalTo (Sub e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (VInt i2)) <- conclusion p2
    , let p3 = deriveMinus i1 i2
    , (Minus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Sub e1 e2) (VInt i3)) "E-Minus" [p1, p2, p3]
deriveEvalTo (Mul e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (VInt i2)) <- conclusion p2
    , let p3 = deriveTimes i1 i2
    , (Times _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Mul e1 e2) (VInt i3)) "E-Times" [p1, p2, p3]
deriveEvalTo (Less e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VInt i1)) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (VInt i2)) <- conclusion p2
    , let p3 = deriveLess i1 i2
    , (Lt _ _ b) <- conclusion p3
    = pure $ Derivation (EvalTo (Less e1 e2) (VBool b)) "E-Less" [p1, p2, p3]
deriveEvalTo (Ite e1 e2 e3)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VBool True)) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ v2) <- conclusion p2
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) v2) "E-IfT" [p1, p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (VBool False)) <- conclusion p1
    , Just p3 <- deriveEvalTo e3
    , (EvalTo _ v3) <- conclusion p3
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) v3) "E-IfF" [p1, p3]
deriveEvalTo _ = Nothing

main :: IO ()
main = do
    s <- getContents
    let e = parse (alexScanTokens s)
    let derivationTree = deriveEvalTo e
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> print d
