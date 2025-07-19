module Syntax where

data Val
    = VInt Int
    | VBool Bool
    | VClos Env String Exp
    | VFix Env String String Exp
    deriving (Eq)

data Exp
    = I Int
    | B Bool
    | V String
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Less Exp Exp
    | Ite Exp Exp Exp
    | Let String Exp Exp
    | Fun String Exp
    | App Exp Exp
    | Letrec String String Exp Exp
    deriving (Eq)

data Env
    = Empty
    | Snoc Env String Val
    deriving (Eq)

data Judgement = EvalTo Env Exp Val
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
    show (VClos env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
    show (VFix env f x e) = "(" ++ show env ++ ")[rec " ++ f ++ " = fun " ++ x ++ " -> " ++ show e ++ "]"

instance Show Exp where
    show (I n) = show n
    show (B b) = showBool b
    show (V x) = x
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Less e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (Ite e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Let x e1 e2) = "let " ++ x ++ " = " ++ show e1 ++ " in " ++ show e2
    show (Fun x e) = "(fun " ++ x ++ " -> " ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Letrec f x e1 e2) = "let rec " ++ f ++ " = fun " ++ x ++ " -> " ++ show e1 ++ " in " ++ show e2

instance Show Env where
    show Empty            = ""
    show (Snoc Empty x v) = x ++ " = " ++ show v
    show (Snoc env x v)   = show env ++ ", " ++ x ++ " = " ++ show v

instance Show Judgement where
    show (EvalTo env e v) = show env ++ " |- " ++ show e ++ " evalto " ++ show v
    show (Plus n1 n2 n3)  = show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
    show (Minus n1 n2 n3) = show n1 ++ " minus " ++ show n2 ++ " is " ++ show n3
    show (Times n1 n2 n3) = show n1 ++ " times " ++ show n2 ++ " is " ++ show n3
    show (Lt n1 n2 b)     = show n1 ++ " less than " ++ show n2 ++ " is " ++ showBool b
