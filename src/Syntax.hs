module Syntax where

data Nat = Z | S Nat
    deriving (Eq)

data Exp = N Nat | Add Exp Exp | Mul Exp Exp
    deriving (Eq)

data Judgement = Plus Nat Nat Nat
               | Times Nat Nat Nat
               | Step Exp Exp
               | MultiStep Exp Exp

instance Show Nat where
    show Z     = "Z"
    show (S n) = "S(" ++ show n ++ ")"

instance Show Exp where
    show (N n)       = show n
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"

instance Show Judgement where
    show (Plus n1 n2 n3) = show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
    show (Times n1 n2 n3) = show n1 ++ " times " ++ show n2 ++ " is " ++ show n3
    show (Step e1 e2) = show e1 ++ " --> " ++ show e2
    show (MultiStep e1 e2) = show e1 ++ " -*-> " ++ show e2
