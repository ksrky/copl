module Syntax where

data Val
    = VInt Int
    | VBool Bool
    | VClos Env String Exp
    | VFix Env String String Exp
    | VNil
    | VCons Val Val
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
    | Nil
    | Cons Exp Exp
    | Match Exp Clauses
    deriving (Eq)

data Pat
    = PVar String
    | PNil
    | PCons Pat Pat
    | PWild
    deriving (Eq)

data Clauses
    = Clause Pat Exp
    | Clauses Pat Exp Clauses
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
               | Matches Pat Val Env
               | MatchFail Pat Val

showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"

instance Show Val where
    show (VInt n)  = show n
    show (VBool b) = showBool b
    show (VClos env x e) = "(" ++ show env ++ ")[fun " ++ x ++ " -> " ++ show e ++ "]"
    show (VFix env f x e) = "(" ++ show env ++ ")[rec " ++ f ++ " = fun " ++ x ++ " -> " ++ show e ++ "]"
    show VNil = "[]"
    show (VCons v1 v2) = "(" ++ show v1 ++ " :: " ++ show v2 ++ ")"

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
    show Nil = "[]"
    show (Cons e1 e2) = "(" ++ show e1 ++ " :: " ++ show e2 ++ ")"
    show (Match e cls) = "(match " ++ show e ++ " with " ++ show cls ++ ")"

instance Show Pat where
    show (PVar x)      = x
    show PNil          = "[]"
    show (PCons p1 p2) = "(" ++ show p1 ++ " :: " ++ show p2 ++ ")"
    show PWild         = "_"

instance Show Clauses where
    show (Clause p e)      = show p ++ " -> " ++ show e
    show (Clauses p e cls) = show p ++ " -> " ++ show e ++ " | " ++ show cls

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
    show (Matches p v env) = show p ++ " matches " ++ show v ++ " when " ++ "(" ++ show env ++ ")"
    show (MatchFail p v) = show p ++ " doesn't match " ++ show v
