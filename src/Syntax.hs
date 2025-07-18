module Syntax where

data Nat = Z | S Nat

data Exp = N Nat | Add Exp Exp | Mul Exp Exp

data Judgement = EvalTo Exp Nat
               | Plus Nat Nat Nat
               | Times Nat Nat Nat

instance Show Nat where
    show Z     = "Z"
    show (S n) = "S(" ++ show n ++ ")"

instance Show Exp where
    show (N n)       = show n
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

instance Show Judgement where
    show (EvalTo e n)     = show e ++ " evalto " ++ show n
    show (Plus n1 n2 n3)  = show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
    show (Times n1 n2 n3) = show n1 ++ " times " ++ show n2 ++ " is " ++ show n3