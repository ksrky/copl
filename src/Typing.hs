module Typing where

import           Data.List (intercalate)
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

instance Instantiate Derivation where
    instantiate d@Derivation{conclusion = Check env e ty} = do
        ty' <- instantiate ty
        env' <- instantiate env
        premises' <- mapM instantiate (premises d)
        return $ d{conclusion = Check env' e ty', premises = premises'}

lookupEnv :: Env -> String -> Maybe Typ
lookupEnv Empty _ = Nothing
lookupEnv (Snoc env x ty) y
    | x == y   = Just ty
    | otherwise = lookupEnv env y

-- Type checking with unification
deriveCheck :: Env -> Exp -> Typ -> IO (Maybe Derivation)
deriveCheck env (I i) expectedTy = do
    success <- unify TInt expectedTy
    if success
        then return $ Just $ Derivation (Check env (I i) TInt) "T-Int" []
        else return Nothing

deriveCheck env (B b) expectedTy = do
    success <- unify TBool expectedTy
    if success
        then return $ Just $ Derivation (Check env (B b) TBool) "T-Bool" []
        else return Nothing

deriveCheck env (V x) expectedTy =
    case lookupEnv env x of
        Nothing -> return Nothing
        Just varTy -> do
            success <- unify varTy expectedTy
            if success
                then return $ Just $ Derivation (Check env (V x) varTy) "T-Var" []
                else return Nothing

deriveCheck env (Add e1 e2) expectedTy = do
    success <- unify TInt expectedTy
    if not success
        then return Nothing
        else do
            p1 <- deriveCheck env e1 TInt
            case p1 of
                Nothing -> return Nothing
                Just d1 -> do
                    p2 <- deriveCheck env e2 TInt
                    case p2 of
                        Nothing -> return Nothing
                        Just d2 -> return $ Just $ Derivation (Check env (Add e1 e2) TInt) "T-Plus" [d1, d2]

deriveCheck env (Sub e1 e2) expectedTy = do
    success <- unify TInt expectedTy
    if not success
        then return Nothing
        else do
            p1 <- deriveCheck env e1 TInt
            case p1 of
                Nothing -> return Nothing
                Just d1 -> do
                    p2 <- deriveCheck env e2 TInt
                    case p2 of
                        Nothing -> return Nothing
                        Just d2 -> return $ Just $ Derivation (Check env (Sub e1 e2) TInt) "T-Minus" [d1, d2]

deriveCheck env (Mul e1 e2) expectedTy = do
    success <- unify TInt expectedTy
    if not success
        then return Nothing
        else do
            p1 <- deriveCheck env e1 TInt
            case p1 of
                Nothing -> return Nothing
                Just d1 -> do
                    p2 <- deriveCheck env e2 TInt
                    case p2 of
                        Nothing -> return Nothing
                        Just d2 -> return $ Just $ Derivation (Check env (Mul e1 e2) TInt) "T-Times" [d1, d2]

deriveCheck env (Less e1 e2) expectedTy = do
    success <- unify TBool expectedTy
    if not success
        then return Nothing
        else do
            p1 <- deriveCheck env e1 TInt
            case p1 of
                Nothing -> return Nothing
                Just d1 -> do
                    p2 <- deriveCheck env e2 TInt
                    case p2 of
                        Nothing -> return Nothing
                        Just d2 -> return $ Just $ Derivation (Check env (Less e1 e2) TBool) "T-Lt" [d1, d2]

deriveCheck env (Ite e1 e2 e3) expectedTy = do
    p1 <- deriveCheck env e1 TBool
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            p2 <- deriveCheck env e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> do
                    p3 <- deriveCheck env e3 expectedTy
                    case p3 of
                        Nothing -> return Nothing
                        Just d3 -> return $ Just $ Derivation (Check env (Ite e1 e2 e3) expectedTy) "T-If" [d1, d2, d3]

deriveCheck env (Let x e1 e2) expectedTy = do
    tv <- newTVar
    p1 <- deriveCheck env e1 tv
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            let Check _ _ ty1 = conclusion d1
            inferredTy1 <- instantiate ty1
            let env' = Snoc env x inferredTy1
            p2 <- deriveCheck env' e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> return $ Just $ Derivation (Check env (Let x e1 e2) expectedTy) "T-Let" [d1, d2]

deriveCheck env (Fun x e) expectedTy = do
    argTy <- newTVar
    retTy <- newTVar
    let funTy = TFun argTy retTy
    success <- unify funTy expectedTy
    if not success
        then return Nothing
        else do
            let env' = Snoc env x argTy
            p <- deriveCheck env' e retTy
            case p of
                Nothing -> return Nothing
                Just d -> do
                    finalArgTy <- instantiate argTy
                    finalRetTy <- instantiate retTy
                    let finalFunTy = TFun finalArgTy finalRetTy
                    return $ Just $ Derivation (Check env (Fun x e) finalFunTy) "T-Fun" [d]

deriveCheck env (App e1 e2) expectedTy = do
    argTy <- newTVar
    let funTy = TFun argTy expectedTy
    p1 <- deriveCheck env e1 funTy
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            p2 <- deriveCheck env e2 argTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> return $ Just $ Derivation (Check env (App e1 e2) expectedTy) "T-App" [d1, d2]

deriveCheck env Nil expectedTy = do
    elemTy <- newTVar
    let listTy = TList elemTy
    success <- unify listTy expectedTy
    if success
        then return $ Just $ Derivation (Check env Nil listTy) "T-Nil" []
        else return Nothing

deriveCheck env (Cons e1 e2) expectedTy = do
    elemTy <- newTVar
    let listTy = TList elemTy
    success <- unify listTy expectedTy
    if not success
        then return Nothing
        else do
            p1 <- deriveCheck env e1 elemTy
            case p1 of
                Nothing -> return Nothing
                Just d1 -> do
                    p2 <- deriveCheck env e2 listTy
                    case p2 of
                        Nothing -> return Nothing
                        Just d2 -> return $ Just $ Derivation (Check env (Cons e1 e2) listTy) "T-Cons" [d1, d2]

deriveCheck env (Match e1 e2 x y e3) expectedTy = do
    elemTy <- newTVar
    let listTy = TList elemTy
    p1 <- deriveCheck env e1 listTy
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            p2 <- deriveCheck env e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> do
                    let env' = Snoc (Snoc env x elemTy) y listTy
                    p3 <- deriveCheck env' e3 expectedTy
                    case p3 of
                        Nothing -> return Nothing
                        Just d3 -> return $ Just $ Derivation (Check env (Match e1 e2 x y e3) expectedTy) "T-Match" [d1, d2, d3]

deriveCheck env (Letrec f x e1 e2) expectedTy = do
    argTy <- newTVar
    retTy <- newTVar
    let funTy = TFun argTy retTy
    let env1 = Snoc (Snoc env f funTy) x argTy
    p1 <- deriveCheck env1 e1 retTy
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            inferredFunTy <- instantiate funTy
            let env2 = Snoc env f inferredFunTy
            p2 <- deriveCheck env2 e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> return $ Just $ Derivation (Check env (Letrec f x e1 e2) expectedTy) "T-LetRec" [d1, d2]

main :: IO ()
main = do
    s <- getContents
    let (env, e, ty) = parse (alexScanTokens s)
    derivationTree <- deriveCheck env e ty
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> do
            finalD <- instantiate d
            print finalD
