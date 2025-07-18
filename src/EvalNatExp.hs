{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module EvalNatExp where

import Data.List (intercalate)

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

deriveEvalTo :: Exp -> Derivation
deriveEvalTo (N n) = Derivation (EvalTo (N n) n) "E-Const" []
deriveEvalTo (Add e1 e2) =
    let p1 = deriveEvalTo e1
        (EvalTo _ n1) = conclusion p1
        p2 = deriveEvalTo e2
        (EvalTo _ n2) = conclusion p2
        p3 = derivePlus n1 n2
        (Plus _ _ n) = conclusion p3
    in Derivation (EvalTo (Add e1 e2) n) "E-Plus" [p1, p2, p3]
deriveEvalTo (Mul e1 e2) =
    let p1 = deriveEvalTo e1
        (EvalTo _ n1) = conclusion p1
        p2 = deriveEvalTo e2
        (EvalTo _ n2) = conclusion p2
        p3 = deriveTimes n1 n2
        (Times _ _ n) = conclusion p3
    in Derivation (EvalTo (Mul e1 e2) n) "E-Times" [p1, p2, p3]

main :: IO ()
main = do
    s <- getContents
    let (exp, nat) = parse (alexScanTokens s)
    let derivationTree = deriveEvalTo exp
    print derivationTree

{-data Nat
    = Z
    | S Nat
    deriving (Eq)

instance Show Nat where
    show Z     = "Z"
    show (S n) = "S(" ++ show n ++ ")"

data Exp
    = Const Nat
    | Plus Exp Exp
    | Times Exp Exp
    deriving (Eq)

instance Show Exp where
    show (Const n)     = show n
    show (Plus e1 e2)  = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Times e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

data NatRules
    = PZero Nat
    | PSucc Nat Nat Nat NatRules
    | TZero Nat
    | TSucc Nat Nat Nat NatRules
    deriving (Eq)

instance Show NatRules where
    show (PZero n) = "Z plus " ++ show n ++ " is " ++ show n ++ " by P-Zero {}"
    show (PSucc n1 n2 n r) =
        "S(" ++ show n1 ++ ") plus " ++ show n2 ++ " is S(" ++ show n ++ ") by P-Succ {" ++ show r ++ "}"
    show (TZero n) = "Z times " ++ show n ++ " is Z by T-Zero {}"
    show (TSucc n1 n2 n4 r) =
        "S(" ++ show n1 ++ ") times " ++ show n2 ++ " is " ++ show n4 ++ "by T-Succ {" ++ show r ++ "}"

data ExpRules
    = EConst Nat
    | EAdd Exp Exp Nat ExpRules ExpRules NatRules
    | EMul Exp Exp Nat ExpRules ExpRules NatRules
    deriving (Eq)

instance Show ExpRules where
    show (EConst n) = show n ++ " evalto " ++ show n ++ " by E-Const {}"
    show (EAdd e1 e2 n r1 r2 r3) =
        show e1 ++ "+" ++ show e2 ++ " evalto " ++ show n ++ " by E-Plus {" ++ show r1 ++ ";" ++ show r2 ++ ";" ++ show r3 ++ "}"
    show (EMul e1 e2 n r1 r2 r3) =
        show e1 ++ "*" ++ show e2 ++ " evalto " ++ show n ++ " by E-Mul {" ++ show r1 ++ ";" ++ show r2 ++ ";" ++ show r3 ++ "}"

evalNat :: Nat -> NatRules

eval :: Exp -> ExpRules
eval (Const n) = EConst n
eval (Plus e1 e2)  =
    let (s1, n1) = eval e1
        (s2, n2) = eval e2
     in ()
eval (Times e1 e2)  =
    let (s1, n1) = eval e1
     in undefined-}
