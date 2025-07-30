module Syntax where

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

type Idx = Int

data Exp'
    = I' Int
    | B' Bool
    | V' Idx
    | Add' Exp' Exp'
    | Sub' Exp' Exp'
    | Mul' Exp' Exp'
    | Less' Exp' Exp'
    | Ite' Exp' Exp' Exp'
    | Let' Exp' Exp'
    | Fun' Exp'
    | App' Exp' Exp'
    | Letrec' Exp' Exp'
    deriving (Eq)

data Env
    = Empty
    | Snoc Env String
    deriving (Eq)

data Judgement = Tr Env Exp Exp'

showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"

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

instance Show Exp' where
    show (I' n) = show n
    show (B' b) = showBool b
    show (V' i) = "#" ++ show i
    show (Add' e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (Sub' e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
    show (Mul' e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
    show (Less' e1 e2) = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (Ite' e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (Let' e1 e2) = "let . = " ++ show e1 ++ " in " ++ show e2
    show (Fun' e) = "(fun . -> " ++ show e ++ ")"
    show (App' e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Letrec' e1 e2) = "let rec . = fun . -> " ++ show e1 ++ " in " ++ show e2

instance Show Env where
    show Empty          = ""
    show (Snoc Empty x) = x
    show (Snoc env x)   = show env ++ ", " ++ x

instance Show Judgement where
    show (Tr env e e') = show env ++ " |- " ++ show e ++ " ==> " ++ show e'
