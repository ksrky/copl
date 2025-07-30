module Syntax where

import           Data.IORef

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
    | Match Exp Exp String String Exp
    deriving (Eq)

type TyRef = IORef (Maybe Typ)

newTyRef :: IO TyRef
newTyRef = newIORef Nothing

readTyRef :: TyRef -> IO (Maybe Typ)
readTyRef = readIORef

writeTyRef :: TyRef -> Typ -> IO ()
writeTyRef ref ty = writeIORef ref (Just ty)

newTVar :: IO Typ
newTVar = TVar <$> newTyRef

data Typ
    = TBool
    | TInt
    | TFun Typ Typ
    | TList Typ
    | TVar TyRef

instance Eq Typ where
    TBool == TBool               = True
    TInt == TInt                 = True
    (TFun t1 t2) == (TFun t3 t4) = t1 == t3 && t2 == t4
    (TList t1) == (TList t2)     = t1 == t2
    (TVar r1) == (TVar r2)       = r1 == r2
    _ == _                       = False

data Env
    = Empty
    | Snoc Env String Typ

instance Eq Env where
    Empty == Empty = True
    (Snoc env1 x1 ty1) == (Snoc env2 x2 ty2) =
        x1 == x2 && ty1 == ty2 && env1 == env2
    _ == _ = False

data Judgement = Check Env Exp Typ

-- Unification algorithm
unify :: Typ -> Typ -> IO Bool
unify t1 t2 = do
    t1' <- deref t1
    t2' <- deref t2
    unify' t1' t2'
  where
    unify' (TVar ref1) (TVar ref2)
        | ref1 == ref2 = return True
        | otherwise = do
            writeTyRef ref1 (TVar ref2)
            return True
    unify' (TVar ref) ty = do
        occurs <- occursCheck ref ty
        if occurs
            then return False
            else do
                writeTyRef ref ty
                return True
    unify' ty (TVar ref) = do
        occurs <- occursCheck ref ty
        if occurs
            then return False
            else do
                writeTyRef ref ty
                return True
    unify' TBool TBool = return True
    unify' TInt TInt = return True
    unify' (TFun ta1 ta2) (TFun tb1 tb2) = do
        b1 <- unify ta1 tb1
        if b1
            then unify ta2 tb2
            else return False
    unify' (TList ta) (TList tb) = unify ta tb
    unify' _ _ = return False

-- Dereference type variables
deref :: Typ -> IO Typ
deref (TVar ref) = do
    mTy <- readTyRef ref
    case mTy of
        Nothing -> return (TVar ref)
        Just ty -> deref ty
deref ty = return ty

-- Occurs check to prevent infinite types
occursCheck :: TyRef -> Typ -> IO Bool
occursCheck targetRef ty = do
    ty' <- deref ty
    occursCheck' targetRef ty'
  where
    occursCheck' target (TVar ref') = return (target == ref')
    occursCheck' target (TFun ta tb) = do
        b1 <- occursCheck' target ta
        if b1
            then return True
            else occursCheck' target tb
    occursCheck' target (TList t) = occursCheck' target t
    occursCheck' _ _ = return False

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
    show Nil = "[]"
    show (Cons e1 e2) = "(" ++ show e1 ++ " :: " ++ show e2 ++ ")"
    show (Match e1 e2 x y e3) = "(match " ++ show e1 ++ " with [] -> " ++ show e2 ++ " | " ++ x ++ " :: " ++ y ++ " -> " ++ show e3 ++ ")"

instance Show Typ where
    show TBool          = "bool"
    show TInt           = "int"
    show (TFun ty1 ty2) = "(" ++ show ty1 ++ " -> " ++ show ty2 ++ ")"
    show (TList ty)     = show ty ++ " list"
    show (TVar _)       = "?"

instance Show Env where
    show Empty             = ""
    show (Snoc Empty x ty) = x ++ " : " ++ show ty
    show (Snoc env x ty)   = show env ++ ", " ++ x ++ " : " ++ show ty

instance Show Judgement where
    show (Check env e ty) = show env ++ " |- " ++ show e ++ " : " ++ show ty

class Instantiate a where
    instantiate :: a -> IO a

instance Instantiate Typ where
    instantiate ty = deref ty >>= instantiate'
      where
        instantiate' (TVar ref) = do
            mTy <- readTyRef ref
            case mTy of
                Just innerTy -> instantiate innerTy
                Nothing      -> return (TVar ref)
        instantiate' (TFun ty1 ty2) = TFun <$> instantiate ty1 <*> instantiate ty2
        instantiate' (TList innerTy) = TList <$> instantiate innerTy
        instantiate' otherTy = return otherTy

instance Instantiate Env where
    instantiate Empty = return Empty
    instantiate (Snoc env x ty) = do
        ty' <- instantiate ty
        env' <- instantiate env
        return $ Snoc env' x ty'

instance Instantiate Judgement where
    instantiate (Check env e ty) = do
        env' <- instantiate env
        ty' <- instantiate ty
        return $ Check env' e ty'
