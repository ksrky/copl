module Syntax where

data Val = VInt Int | VBool Bool
    deriving (Eq)

data Res = RVal Val | RError
    deriving (Eq)

data Exp = I Int | B Bool | Add Exp Exp | Sub Exp Exp | Mul Exp Exp | Less Exp Exp | Ite Exp Exp Exp
    deriving (Eq)

data Judgement = EvalTo Exp Val
               | Plus Int Int Int
               | Minus Int Int Int
               | Times Int Int Int
               | Lt Int Int Bool

showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"

instance Show Val where
    show (VInt n)  = show n
    show (VBool b) = showBool b

instance Show Res where
    show (RVal v) = show v
    show RError   = "error"

instance Show Exp where
    show (I n)       = show n
    show (B b) = showBool b
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Less e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (Ite e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3

instance Show Judgement where
    show (EvalTo e n)     = show e ++ " evalto " ++ show n
    show (Plus n1 n2 n3)  = show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
    show (Minus n1 n2 n3) = show n1 ++ " minus " ++ show n2 ++ " is " ++ show n3
    show (Times n1 n2 n3) = show n1 ++ " times " ++ show n2 ++ " is " ++ show n3
    show (Lt n1 n2 b)     = show n1 ++ " less than " ++ show n2 ++ " is " ++ showBool b
