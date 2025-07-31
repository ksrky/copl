module Syntax where

import           Data.IORef
import           System.IO.Unsafe (unsafePerformIO)

-- Global counter for generating fresh type variable names
{-# NOINLINE tyVarCounter #-}
tyVarCounter :: IORef Int
tyVarCounter = unsafePerformIO (newIORef 0)

freshTyVarName :: IO TyVarName
freshTyVarName = do
    n <- readIORef tyVarCounter
    writeIORef tyVarCounter (n + 1)
    return $ "a" ++ show n

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
type TyVarName = String

-- Type scheme: forall α₁...αₙ. τ
data TypeScheme = Forall [TyVarName] Typ
    deriving (Eq)

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
    | TNamed TyVarName  -- Named type variable for schemes

instance Eq Typ where
    TBool == TBool               = True
    TInt == TInt                 = True
    (TFun t1 t2) == (TFun t3 t4) = t1 == t3 && t2 == t4
    (TList t1) == (TList t2)     = t1 == t2
    (TVar r1) == (TVar r2)       = r1 == r2
    (TNamed n1) == (TNamed n2)   = n1 == n2
    _ == _                       = False

data Env
    = Empty
    | Snoc Env String TypeScheme

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
    unify' (TVar ref) (TNamed name) = do
        writeTyRef ref (TNamed name)
        return True
    unify' (TNamed name) (TVar ref) = do
        writeTyRef ref (TNamed name)
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
    unify' (TNamed n1) (TNamed n2) = return (n1 == n2)
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
    show (TVar _)       = "?"  -- Simplified for now
    show (TNamed name)  = "'" ++ name

instance Show TypeScheme where
    show (Forall [] ty)   = show ty
    show (Forall vars ty) = unwords (map ("'"++) vars) ++ ". " ++ show ty

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

instance Instantiate TypeScheme where
    instantiate (Forall vars ty) = do
        -- Create fresh type variables for each quantified variable
        freshVars <- mapM (\_ -> newTVar) vars
        let subst = zip vars freshVars
        ty' <- substituteInType subst ty
        return $ Forall [] ty'
      where
        substituteInType :: [(TyVarName, Typ)] -> Typ -> IO Typ
        substituteInType subst (TNamed name) =
            case lookup name subst of
                Just newTy -> return newTy
                Nothing    -> return (TNamed name)
        substituteInType subst (TFun t1 t2) = do
            t1' <- substituteInType subst t1
            t2' <- substituteInType subst t2
            return $ TFun t1' t2'
        substituteInType subst (TList t) = do
            t' <- substituteInType subst t
            return $ TList t'
        substituteInType _ t = return t

instance Instantiate Env where
    instantiate Empty = return Empty
    instantiate (Snoc env x scheme) = do
        scheme' <- instantiate scheme
        env' <- instantiate env
        return $ Snoc env' x scheme'

instance Instantiate Judgement where
    instantiate (Check env e ty) = do
        env' <- instantiate env
        ty' <- instantiate ty
        return $ Check env' e ty'

-- Free type variables in a type
freeVarsInType :: Typ -> IO [TyVarName]
freeVarsInType ty = do
    ty' <- deref ty
    freeVarsInType' ty'
  where
    freeVarsInType' (TNamed name) = return [name]
    freeVarsInType' (TFun t1 t2) = do
        fv1 <- freeVarsInType t1
        fv2 <- freeVarsInType t2
        return $ fv1 ++ fv2
    freeVarsInType' (TList t) = freeVarsInType t
    freeVarsInType' _ = return []

-- Free type variables in an environment
freeVarsInEnv :: Env -> IO [TyVarName]
freeVarsInEnv Empty = return []
freeVarsInEnv (Snoc env _ (Forall _ ty)) = do
    envFV <- freeVarsInEnv env
    tyFV <- freeVarsInType ty
    return $ envFV ++ tyFV

-- Generalize a type to a type scheme
generalize :: Env -> Typ -> IO TypeScheme
generalize env ty = do
    envFV <- freeVarsInEnv env
    tyFV <- freeVarsInType ty
    let quantVars = filter (`notElem` envFV) tyFV
    return $ Forall quantVars ty

-- Instantiate a type scheme to a type
instantiateScheme :: TypeScheme -> IO Typ
instantiateScheme (Forall vars ty) = do
    freshVars <- mapM (\_ -> newTVar) vars
    let subst = zip vars freshVars
    substituteInType subst ty
  where
    substituteInType :: [(TyVarName, Typ)] -> Typ -> IO Typ
    substituteInType subst (TNamed name) =
        case lookup name subst of
            Just newTy -> return newTy
            Nothing    -> return (TNamed name)
    substituteInType subst (TFun t1 t2) = do
        t1' <- substituteInType subst t1
        t2' <- substituteInType subst t2
        return $ TFun t1' t2'
    substituteInType subst (TList t) = do
        t' <- substituteInType subst t
        return $ TList t'
    substituteInType _ t = return t

-- Helper function to remove duplicates from a list
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (/= x) xs)

-- Normalize type variables for display
normalizeTyVars :: Typ -> IO Typ
normalizeTyVars ty = do
    ty' <- instantiate ty
    normalizeTyVars' ty'
  where
    normalizeTyVars' (TVar _) = do
        -- Replace unresolved TVar with a named variable
        name <- freshTyVarName
        return $ TNamed name
    normalizeTyVars' (TFun t1 t2) = do
        t1' <- normalizeTyVars' t1
        t2' <- normalizeTyVars' t2
        return $ TFun t1' t2'
    normalizeTyVars' (TList t) = do
        t' <- normalizeTyVars' t
        return $ TList t'
    normalizeTyVars' t = return t
