{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Reduce where

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

derivePlus :: Nat -> Nat -> Derivation
derivePlus Z n2 = Derivation (Plus Z n2 n2) "P-Zero" []
derivePlus (S n1) n2 =
    let p = derivePlus n1 n2
        (Plus _ _ n) = conclusion p
    in Derivation (Plus (S n1) n2 (S n)) "P-Succ" [p]

deriveTimes :: Nat -> Nat -> Derivation
deriveTimes Z n2 = Derivation (Times Z n2 Z) "T-Zero" []
deriveTimes (S n1) n2 =
    let p1 = deriveTimes n1 n2
        (Times _ _ n3) = conclusion p1
        p2 = derivePlus n2 n3
        (Plus _ _ n4) = conclusion p2
    in Derivation (Times (S n1) n2 n4) "T-Succ" [p1, p2]

-- ユーティリティ: 式が値か判定
isValue :: Exp -> Bool
isValue (N _) = True
isValue _     = False

-- 1ステップ評価(->)の導出木を生成 (R-* rules)
deriveStep :: Exp -> Maybe Derivation
-- R-PLUS
deriveStep (Add (N n1) (N n2)) =
    let p1 = derivePlus n1 n2
        (Plus _ _ n3) = conclusion p1
    in Just (Derivation (Step (Add (N n1) (N n2)) (N n3)) "R-Plus" [p1])
-- R-PLUSL
deriveStep (Add e1 e2) | not (isValue e1) = do
    p1 <- deriveStep e1
    let (Step _ e1') = conclusion p1
    Just (Derivation (Step (Add e1 e2) (Add e1' e2)) "R-PlusL" [p1])
-- R-PLUSR
deriveStep (Add e1@(N _) e2) | not (isValue e2) = do
    p1 <- deriveStep e2
    let (Step _ e2') = conclusion p1
    Just (Derivation (Step (Add e1 e2) (Add e1 e2')) "R-PlusR" [p1])
-- 値は進めない
deriveStep (N _) = Nothing
-- (乗算ルールは今回不要なため省略)
deriveStep _ = Nothing

-- 複数ステップ評価(->*)の導出木を生成 (MR-* rules)

-- ... Nat, Exp, Judgement, Derivation, Showインスタンス, derivePlus, deriveStep は変更なし ...

-- 複数ステップ評価(->*)の導出木を生成する関数を修正
-- 入力は開始時の式 Exp のみ
deriveMultiStep :: Exp -> Maybe Derivation
deriveMultiStep e =
    -- まず、式eが1ステップ進めるか試す
    case deriveStep e of
        -- Case 1: 1ステップも進めない場合 (eが値)
        Nothing ->
            -- 式がNat値であることを確認し、MR-ZEROルールを適用
            case e of
                N n -> Just (Derivation (MultiStep (N n) (N n)) "MR-Zero" [])
                _   -> Nothing -- 値ではないのに停止した場合 (エラー)

        -- Case 2: 1ステップ進める場合 (e -> e_mid)
        Just p1_step -> do
            -- p1_step は e -> e_mid の導出木
            let (Step _ e_mid) = conclusion p1_step

            -- e_mid から最終結果までの導出木を再帰的に取得
            -- p2_multistep は e_mid ->* final_val の導出木
            p2_multistep <- deriveMultiStep e_mid

            -- p2_multistepから最終的な値を取得
            let (MultiStep _ final_val) = conclusion p2_multistep

            -- MR-MULTIルールを使い、p1とp2を前提として全体の導出木を構築
            Just (Derivation (MultiStep e final_val) "MR-Multi" [p1_step, p2_multistep])

main :: IO ()
main = do
    s <- getContents
    let e = parse (alexScanTokens s)
    case deriveMultiStep e of
        Just tree -> print tree
        Nothing   -> putStrLn "error"
