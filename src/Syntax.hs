module Syntax where

data Val
    = VInt Int
    | VBool Bool
    | VClos Env Exp
    | VFix Env Exp
    deriving (Eq)

type Idx = Int

data Exp
    = I Int
    | B Bool
    | V Idx
    | Add Exp Exp
    | Sub Exp Exp
    | Mul Exp Exp
    | Less Exp Exp
    | Ite Exp Exp Exp
    | Let Exp Exp
    | Fun Exp
    | App Exp Exp
    | Letrec Exp Exp
    deriving (Eq)

data Env
    = Empty
    | Snoc Env Val
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
    show (VClos env e) = "(" ++ show env ++ ")[fun . -> " ++ show e ++ "]"
    show (VFix env e) = "(" ++ show env ++ ")[rec . = fun . -> " ++ show e ++ "]"

instance Show Exp where
    show (I n) = show n
    show (B b) = showBool b
    show (V i) = "#" ++ show i
    show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Less e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (Ite e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Let e1 e2) = "let . = " ++ show e1 ++ " in " ++ show e2
    show (Fun e) = "(fun . -> " ++ show e ++ ")"
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Letrec e1 e2) = "let rec . = fun . -> " ++ show e1 ++ " in " ++ show e2

instance Show Env where
    show Empty          = ""
    show (Snoc Empty v) = show v
    show (Snoc env v)   = show env ++ ", " ++ show v

instance Show Judgement where
    show (EvalTo env e v) = show env ++ " |- " ++ show e ++ " evalto " ++ show v
    show (Plus n1 n2 n3)  = show n1 ++ " plus " ++ show n2 ++ " is " ++ show n3
    show (Minus n1 n2 n3) = show n1 ++ " minus " ++ show n2 ++ " is " ++ show n3
    show (Times n1 n2 n3) = show n1 ++ " times " ++ show n2 ++ " is " ++ show n3
    show (Lt n1 n2 b)     = show n1 ++ " less than " ++ show n2 ++ " is " ++ showBool b
