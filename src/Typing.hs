module Typing where

import           Data.List (intercalate, groupBy, sortBy, partition)
import           System.IO.Unsafe (unsafePerformIO)
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
        -- Don't instantiate the environment, keep original type schemes
        premises' <- mapM instantiate (premises d)
        return $ d{conclusion = Check env e ty', premises = premises'}

lookupEnv :: Env -> String -> Maybe TypeScheme
lookupEnv Empty _ = Nothing
lookupEnv (Snoc env x scheme) y
    | x == y   = Just scheme
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
        Just varScheme -> do
            varTy <- instantiateScheme varScheme
            success <- unify varTy expectedTy
            if success
                then do
                    -- Resolve the type after unification
                    resolvedTy <- instantiate expectedTy
                    normalizedTy <- normalizeTyVars resolvedTy
                    return $ Just $ Derivation (Check env (V x) normalizedTy) "T-Var" []
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
                        Just d3 -> do
                            -- Normalize types for consistent display
                            finalExpectedTy <- instantiate expectedTy
                            normalizedExpectedTy <- normalizeTyVars finalExpectedTy

                            -- Update premises with resolved types
                            let Check _ _ condTy = conclusion d1
                            resolvedCondTy <- instantiate condTy
                            normalizedCondTy <- normalizeTyVars resolvedCondTy
                            let normalizedD1 = d1{conclusion = Check env e1 normalizedCondTy}

                            let Check _ _ thenTy = conclusion d2
                            resolvedThenTy <- instantiate thenTy
                            normalizedThenTy <- normalizeTyVars resolvedThenTy
                            let normalizedD2 = d2{conclusion = Check env e2 normalizedThenTy}

                            let Check _ _ elseTy = conclusion d3
                            resolvedElseTy <- instantiate elseTy
                            normalizedElseTy <- normalizeTyVars resolvedElseTy
                            let normalizedD3 = d3{conclusion = Check env e3 normalizedElseTy}

                            return $ Just $ Derivation (Check env (Ite e1 e2 e3) normalizedExpectedTy) "T-If" [normalizedD1, normalizedD2, normalizedD3]

deriveCheck env (Let x e1 e2) expectedTy = do
    tv <- newTVar
    p1 <- deriveCheck env e1 tv
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            let Check _ _ ty1 = conclusion d1
            -- Get the actual inferred type after unification
            inferredTy1 <- instantiate ty1
            -- Generalize the type with respect to the current environment
            generalizedTy1 <- generalize env inferredTy1
            let env' = Snoc env x generalizedTy1
            p2 <- deriveCheck env' e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> do
                    -- Normalize types in the final derivation for display
                    finalTy1 <- instantiate ty1
                    normalizedTy1 <- normalizeTyVars finalTy1
                    let normalizedD1 = d1{conclusion = Check env e1 normalizedTy1}
                    return $ Just $ Derivation (Check env (Let x e1 e2) expectedTy) "T-Let" [normalizedD1, d2]

deriveCheck env (Fun x e) expectedTy = do
    argTy <- newTVar
    retTy <- newTVar
    let funTy = TFun argTy retTy
    success <- unify funTy expectedTy
    if not success
        then return Nothing
        else do
            -- Keep the type variable for the argument (no early resolution)
            let argScheme = Forall [] argTy
            let env' = Snoc env x argScheme
            p <- deriveCheck env' e retTy
            case p of
                Nothing -> return Nothing
                Just d -> do
                    -- Get the final resolved types after all unifications
                    finalArgTy <- instantiate argTy
                    finalRetTy <- instantiate retTy
                    let finalFunTy = TFun finalArgTy finalRetTy
                    normalizedFunTy <- normalizeTyVars finalFunTy
                    -- Also normalize the premise with resolved argument type
                    let Check _ _ retTyFromPremise = conclusion d
                    resolvedRetTy <- instantiate retTyFromPremise
                    normalizedRetTy <- normalizeTyVars resolvedRetTy
                    let normalizedD = d{conclusion = Check env' e normalizedRetTy}
                    return $ Just $ Derivation (Check env (Fun x e) normalizedFunTy) "T-Abs" [normalizedD]

deriveCheck env (App e1 e2) expectedTy = do
    argTy <- newTVar
    let funTy = TFun argTy expectedTy
    p1 <- deriveCheck env e1 funTy
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            -- Get the resolved argument type after e1's type checking
            resolvedArgTy <- instantiate argTy
            p2 <- deriveCheck env e2 resolvedArgTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> do
                    -- Get final resolved types for display
                    finalExpectedTy <- instantiate expectedTy
                    -- Normalize types for consistent display
                    normalizedExpectedTy <- normalizeTyVars finalExpectedTy
                    -- Update premises with resolved types
                    let Check _ _ funTyFromPremise = conclusion d1
                    resolvedFunTy <- instantiate funTyFromPremise
                    normalizedFunTy <- normalizeTyVars resolvedFunTy
                    let normalizedD1 = d1{conclusion = Check env e1 normalizedFunTy}

                    let Check _ _ argTyFromPremise = conclusion d2
                    resolvedArgTyFromPremise <- instantiate argTyFromPremise
                    normalizedArgTyFromPremise <- normalizeTyVars resolvedArgTyFromPremise
                    let normalizedD2 = d2{conclusion = Check env e2 normalizedArgTyFromPremise}

                    return $ Just $ Derivation (Check env (App e1 e2) normalizedExpectedTy) "T-App" [normalizedD1, normalizedD2]

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
                    resolvedElemTy <- instantiate elemTy
                    resolvedListTy <- instantiate listTy
                    let elemScheme = Forall [] resolvedElemTy
                    let listScheme = Forall [] resolvedListTy
                    let env' = Snoc (Snoc env x elemScheme) y listScheme
                    p3 <- deriveCheck env' e3 expectedTy
                    case p3 of
                        Nothing -> return Nothing
                        Just d3 -> return $ Just $ Derivation (Check env (Match e1 e2 x y e3) expectedTy) "T-Match" [d1, d2, d3]

deriveCheck env (Letrec f x e1 e2) expectedTy = do
    argTy <- newTVar
    retTy <- newTVar
    let funTy = TFun argTy retTy
    let funScheme = Forall [] funTy
    let argScheme = Forall [] argTy
    let env1 = Snoc (Snoc env f funScheme) x argScheme
    p1 <- deriveCheck env1 e1 retTy
    case p1 of
        Nothing -> return Nothing
        Just d1 -> do
            resolvedFunTy <- instantiate funTy
            generalizedFunTy <- generalize env resolvedFunTy
            let env2 = Snoc env f generalizedFunTy
            p2 <- deriveCheck env2 e2 expectedTy
            case p2 of
                Nothing -> return Nothing
                Just d2 -> return $ Just $ Derivation (Check env (Letrec f x e1 e2) expectedTy) "T-LetRec" [d1, d2]

main :: IO ()
main = do
    s <- getContents
    let (env, e, ty) = parse (alexScanTokens s)
    resetTyVarCounter  -- Reset counter for consistent naming
    putStrLn $ "Parsed environment: " ++ show env
    putStrLn $ "Parsed expression: " ++ show e
    putStrLn $ "Expected type: " ++ show ty
    derivationTree <- deriveCheck env e ty
    case derivationTree of
        Nothing -> putStrLn "No valid derivation found."
        Just d  -> do
            finalD <- instantiate d
            resetTyVarCounter  -- Reset for consistent display naming
            normalizedD <- normalizeDerivationGlobally finalD
            print normalizedD

-- Normalize all types in a derivation tree globally for consistent naming
normalizeDerivationGlobally :: Derivation -> IO Derivation
normalizeDerivationGlobally d = do
    -- Collect all type references in the derivation tree
    tyRefs <- collectAllTyRefs d
    -- Create a mapping that considers unification chains
    mapping <- createUnificationAwareMapping tyRefs
    -- Apply the mapping to the entire tree
    applyMappingToDerivation mapping d
  where
    collectAllTyRefs :: Derivation -> IO [TyRef]
    collectAllTyRefs (Derivation (Check env _ ty) _ subPremises) = do
        tyRefs <- collectTyVars ty
        envRefs <- collectEnvTyRefs env
        premiseRefs <- concat <$> mapM collectAllTyRefs subPremises
        return $ nub (tyRefs ++ envRefs ++ premiseRefs)

    collectEnvTyRefs :: Env -> IO [TyRef]
    collectEnvTyRefs Empty = return []
    collectEnvTyRefs (Snoc env _ (Forall _ ty)) = do
        envRefs <- collectEnvTyRefs env
        tyRefs <- collectTyVars ty
        return $ nub (envRefs ++ tyRefs)

    -- Create a mapping that considers unification chains
    createUnificationAwareMapping :: [TyRef] -> IO [(TyRef, TyVarName)]
    createUnificationAwareMapping refs = do
        -- Create equivalence groups based on final references
        groups <- groupByEquivalence refs
        -- Assign names to each group
        assignments <- mapM assignNameToGroup groups
        return $ concat assignments
      where
        groupByEquivalence :: [TyRef] -> IO [[TyRef]]
        groupByEquivalence allRefs = do
            -- Build equivalence relation
            pairs <- mapM (\ref -> do
                final <- findFinalRef ref
                return (ref, final)) allRefs
            -- Group refs with same final reference
            return $ groupEquivalent pairs []

        groupEquivalent :: [(TyRef, TyRef)] -> [[TyRef]] -> [[TyRef]]
        groupEquivalent [] groups = groups
        groupEquivalent ((ref, final):rest) groups =
            let (sameGroup, otherGroups) = partition (hasSameFinal final) groups
                newGroup = ref : concat sameGroup
            in groupEquivalent rest (newGroup : otherGroups)

        hasSameFinal :: TyRef -> [TyRef] -> Bool
        hasSameFinal final group = case group of
            [] -> False
            (ref:_) -> unsafePerformIO $ do
                refFinal <- findFinalRef ref
                return $ refFinal == final

        assignNameToGroup :: [TyRef] -> IO [(TyRef, TyVarName)]
        assignNameToGroup group = do
            name <- freshTyVarName
            return $ map (\ref -> (ref, name)) group

        findFinalRef :: TyRef -> IO TyRef
        findFinalRef ref = do
            mTy <- readTyRef ref
            case mTy of
                Nothing -> return ref
                Just (TVar ref') -> findFinalRef ref'
                Just _ -> return ref

    applyMappingToDerivation :: [(TyRef, TyVarName)] -> Derivation -> IO Derivation
    applyMappingToDerivation mapping (Derivation (Check env e ty) rule subPremises) = do
        normalizedTy <- applyVarMapping mapping ty
        normalizedEnv <- applyMappingToEnv mapping env
        normalizedPremises <- mapM (applyMappingToDerivation mapping) subPremises
        return $ Derivation (Check normalizedEnv e normalizedTy) rule normalizedPremises

    applyMappingToEnv :: [(TyRef, TyVarName)] -> Env -> IO Env
    applyMappingToEnv _ Empty = return Empty
    applyMappingToEnv mapping (Snoc env x (Forall vars ty)) = do
        normalizedEnv <- applyMappingToEnv mapping env
        normalizedTy <- applyVarMapping mapping ty
        return $ Snoc normalizedEnv x (Forall vars normalizedTy)-- Normalize all types in a derivation tree for better display
normalizeDerivation :: Derivation -> IO Derivation
normalizeDerivation d = do
    let Check env e ty = conclusion d
    normalizedTy <- normalizeTyVars ty
    normalizedPremises <- mapM normalizeDerivation (premises d)
    return $ d { conclusion = Check env e normalizedTy, premises = normalizedPremises }
