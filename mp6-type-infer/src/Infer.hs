module Infer where

import Common

import Control.Monad.Writer (listen)
import Control.Monad.Except (throwError)
import Data.Map.Strict as H (Map, insert, lookup, empty, fromList, singleton)

tag1 = 64002
tag2 = 68869
tag3 = 19545

  {- question 1: fresh instance function -}

freshInst :: PolyTy -> Infer MonoTy
freshInst (Forall qVars tau) = do
  fresh <- mapM (\_ -> freshTau) qVars
  let subs = H.fromList (zip qVars fresh)
  return $ apply subs tau

  {- question 2: occurs check -}

occurs :: VarId -> MonoTy -> Bool
occurs i tau = i `elem` freeVars tau

  {- question 3: unification -}
isTyVar :: MonoTy -> Bool
isTyVar (TyVar _) = True
isTyVar _ = False

unify :: [Constraint] -> Infer Substitution
unify [] = return substEmpty
unify ((s:~:t) : phi')
  | s == t = unify phi'
  | isTyVar t && not (isTyVar s) = unify ((t:~:s) : phi')
unify ((TyConst c1 args1 :~: TyConst c2 args2) : phi')
  | c1 == c2 && length args1 == length args2 = unify (zipWith (:~:) args1 args2 ++ phi')
  | otherwise = throwError (Can'tMatch (TyConst c1 args1) (TyConst c2 args2)) 
unify ((TyVar x :~: t) : phi')
  | occurs x t = throwError (InfiniteType x t)
  | otherwise = do
    let substitution = substInit x t
    let phi'' = map (apply substitution) phi'
    sigma <- unify phi''
    return $ substCompose sigma substitution


  {- question 4: type inference -}

infer :: TypeEnv -> Exp -> Infer MonoTy
-- problem 1
infer env (ConstExp exp) = freshInst (constTySig exp)

-- problem 2
infer env (VarExp exp) = case H.lookup exp env of
  Nothing -> throwError (LookupError exp)
  Just poly -> freshInst poly

-- problem 3
infer env (LetExp str e1 e2) = do
  (tau, constraints) <- listen $ infer env e1
  sigma <- unify constraints
  let poly = gen env (apply sigma tau)
  infer (H.insert str poly env) e2

-- problem 4
infer env (MonOpExp op e) = do
  tau <- infer env e
  tauFresh <- freshTau
  typeSig <- freshInst (monopTySig op)
  constrain (funTy tau tauFresh) typeSig
  return tauFresh

infer env (BinOpExp op e1 e2) = do
  tau1 <- infer env e1
  tau2 <- infer env e2
  tauFresh <- freshTau
  typeSig <- freshInst (binopTySig op)
  constrain (funTy tau1 (funTy tau2 tauFresh)) typeSig
  return tauFresh

-- problem 5
infer env (IfExp e1 e2 e3) = do
  tau1 <- infer env e1
  tau2 <- infer env e2
  tau3 <- infer env e3
  constrain tau1 boolTy
  constrain tau2 tau3
  --no new result, so we dont need the create fresh type
  return tau2

-- problem 6
infer env (FunExp x e) = do
  tau <- freshTau
  let env2 = H.insert x (Forall [] tau) env
  tau2 <- infer env2 e
  return (funTy tau tau2)

-- problem 7
infer env (AppExp e1 e2) = do
  tauFresh <- freshTau
  tau1 <- infer env e1
  tau2 <- infer env e2
  constrain tau1 (funTy tau2 tauFresh)
  return tauFresh

-- problem 8
infer env (LetRecExp f x e1 e2) = do
  tauArg <- freshTau
  tauFunc <- freshTau
  let funcType = funTy tauArg tauFunc
  let envX = H.insert x (Forall [] tauArg) env
  let env2 = H.insert f (Forall [] funcType) envX
  (tau1, phi) <- listen $ infer env2 e1
  sigma <- unify ((tau1 :~: tauFunc) : phi)
  let poly = gen env (apply sigma funcType)
  let env3 = H.insert f poly env
  infer env3 e2


inferInit :: TypeEnv -> Exp -> Infer PolyTy
inferInit env e = do
  (tau, constraints) <- listen $ infer env e
  substitution <- unify constraints
  return $ quantifyMonoTy $ apply substitution tau

inferDec :: TypeEnv -> Dec -> Infer (TypeEnv, PolyTy)
inferDec env (AnonDec e') = do
  tau <- inferInit env e'
  return (env, tau)
inferDec env (LetDec x e') = do
  tau <- inferInit env (LetExp x e' (VarExp x))
  return (H.insert x tau env, tau)
inferDec env (LetRec f x e') = do
  tau <- inferInit env (LetRecExp f x e' (VarExp f))
  return (H.insert f tau env, tau)
