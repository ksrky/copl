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

deriveEvalTo :: Exp -> Maybe Derivation
deriveEvalTo (I i) = pure $ Derivation (EvalTo (I i) (RV (VInt i))) "E-Int" []
deriveEvalTo (B b) = pure $ Derivation (EvalTo (B b) (RV (VBool b))) "E-Bool" []
deriveEvalTo (Add e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VInt i1))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV (VInt i2))) <- conclusion p2
    , let p3 = derivePlus i1 i2
    , (Plus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Add e1 e2) (RV (VInt i3))) "E-Plus" [p1, p2, p3]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV VBool{})) <- conclusion p1
    = Just $ Derivation (EvalTo (Add e1 e2) RError) "E-PlusBoolL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV VBool{})) <- conclusion p2
    = Just $ Derivation (EvalTo (Add e1 e2) RError) "E-PlusBoolR" [p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ RError) <- conclusion p1
    = Just $ Derivation (EvalTo (Add e1 e2) RError) "E-PlusErrorL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ RError) <- conclusion p2
    = Just $ Derivation (EvalTo (Add e1 e2) RError) "E-PlusErrorR" [p2]
deriveEvalTo (Sub e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VInt i1))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV (VInt i2))) <- conclusion p2
    , let p3 = deriveMinus i1 i2
    , (Minus _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Sub e1 e2) (RV (VInt i3))) "E-Minus" [p1, p2, p3]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV VBool{})) <- conclusion p1
    = Just $ Derivation (EvalTo (Sub e1 e2) RError) "E-MinusBoolL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV VBool{})) <- conclusion p2
    = Just $ Derivation (EvalTo (Sub e1 e2) RError) "E-MinusBoolR" [p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ RError) <- conclusion p1
    = Just $ Derivation (EvalTo (Sub e1 e2) RError) "E-MinusErrorL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ RError) <- conclusion p2
    = Just $ Derivation (EvalTo (Sub e1 e2) RError) "E-MinusErrorR" [p2]
deriveEvalTo (Mul e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VInt i1))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV (VInt i2))) <- conclusion p2
    , let p3 = deriveTimes i1 i2
    , (Times _ _ i3) <- conclusion p3
    = pure $ Derivation (EvalTo (Mul e1 e2) (RV (VInt i3))) "E-Times" [p1, p2, p3]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV VBool{})) <- conclusion p1
    = Just $ Derivation (EvalTo (Mul e1 e2) RError) "E-TimesBoolL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV VBool{})) <- conclusion p2
    = Just $ Derivation (EvalTo (Mul e1 e2) RError) "E-TimesBoolR" [p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ RError) <- conclusion p1
    = Just $ Derivation (EvalTo (Mul e1 e2) RError) "E-TimesErrorL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ RError) <- conclusion p2
    = Just $ Derivation (EvalTo (Mul e1 e2) RError) "E-TimesErrorR" [p2]
deriveEvalTo (Less e1 e2)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VInt i1))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV (VInt i2))) <- conclusion p2
    , let p3 = deriveLess i1 i2
    , (Lt _ _ b) <- conclusion p3
    = pure $ Derivation (EvalTo (Less e1 e2) (RV (VBool b))) "E-Lt" [p1, p2, p3]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV VBool{})) <- conclusion p1
    = Just $ Derivation (EvalTo (Less e1 e2) RError) "E-LtBoolL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV VBool{})) <- conclusion p2
    = Just $ Derivation (EvalTo (Less e1 e2) RError) "E-LtBoolR" [p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ RError) <- conclusion p1
    = Just $ Derivation (EvalTo (Less e1 e2) RError) "E-LtErrorL" [p1]
    | Just p2 <- deriveEvalTo e2
    , (EvalTo _ RError) <- conclusion p2
    = Just $ Derivation (EvalTo (Less e1 e2) RError) "E-LtErrorR" [p2]
deriveEvalTo (Ite e1 e2 e3)
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VBool True))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ (RV v2)) <- conclusion p2
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) (RV v2)) "E-IfT" [p1, p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VBool False))) <- conclusion p1
    , Just p3 <- deriveEvalTo e3
    , (EvalTo _ (RV v3)) <- conclusion p3
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) (RV v3)) "E-IfF" [p1, p3]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV VInt{})) <- conclusion p1
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) RError) "E-IfInt" [p1]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ RError) <- conclusion p1
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) RError) "E-IfError" [p1]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VBool True))) <- conclusion p1
    , Just p2 <- deriveEvalTo e2
    , (EvalTo _ RError) <- conclusion p2
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) RError) "E-IfTError" [p1, p2]
    | Just p1 <- deriveEvalTo e1
    , (EvalTo _ (RV (VBool False))) <- conclusion p1
    , Just p3 <- deriveEvalTo e3
    , (EvalTo _ RError) <- conclusion p3
    = Just $ Derivation (EvalTo (Ite e1 e2 e3) RError) "E-IfEError" [p1, p3]
deriveEvalTo _ = Nothing

main :: IO ()
main = do
    s <- getContents
    let e = parse (alexScanTokens s)
    let derivationTree = deriveEvalTo e
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> print d
