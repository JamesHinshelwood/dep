module Check where

import Unbound.LocallyNameless
import Data.Maybe
import Data.Set
import Control.Monad.Trans.Maybe
import Syntax

type Context = [(Name Term, Term)]

addType :: Name Term -> Term -> Context -> Context
addType x ty g = (x, ty) : g

checkType :: Context -> Term -> LFreshM ()
checkType g ty = do
  kind <- typeOf g ty
  case kind of
    Type -> return ()
    _ -> error "type not well formed"

typeOf :: Context -> Term -> LFreshM Term
typeOf _ Type = return Type
typeOf g (Var x) = case lookup x g of
  Just ty -> return ty
  Nothing -> error "var unbound"
typeOf g (Lam b) = lunbind b $ \((x, Embed ty), tm) -> do
  checkType g ty
  tmTy <- typeOf (addType x ty g) tm
  return $ Pi $ bind (x, embed ty) tmTy
typeOf g (App lhs rhs) = do
  lhsTy <- typeOf g lhs
  rhsTy <- typeOf g rhs
  case lhsTy of
    Pi b -> lunbind b $ \((x, Embed argTy), retTy) -> do
      if beq argTy rhsTy
        then return $ subst x rhs retTy
        else error $ "function expected " ++ show argTy ++ " but got " ++ show rhsTy
    _ -> error "tried to apply a non-lambda term"
typeOf g (Pi b) = lunbind b $ \((x, Embed lTy), rTy) -> do
  checkType g lTy
  checkType (addType x lTy g) rTy
  return Type
typeOf g (Let b) = lunbind b $ \((x, Embed t1), t2) -> do
  ty1 <- typeOf g t1
  ty2 <- typeOf (addType x ty1 g) (subst x t1 t2) -- FIXME: subst is ugly here, should be part of nf
  return ty2
typeOf g (Decl b) = lunbind b $ \((x, Embed ty), tm) -> do
  checkType g ty
  tmTy <- typeOf (addType x ty g) tm
  return tmTy
typeOf g (TypedPair t1 t2 ty) = do
  case ty of
    Sigma b -> lunbind b $ \((x, Embed s), t) -> do
      ty1 <- typeOf g t1
      ty2 <- typeOf g t2
      if beq ty1 s && beq ty2 (subst x t1 t)
        then return ty
        else error "bad annotation on pair"
    _ -> error "pair annotation is not a sigma"
typeOf g (Sigma b) = lunbind b $ \((x, Embed s), t) -> do
  checkType g s
  checkType (addType x s g) t
  return Type
typeOf g (First t) = do
  ty <- typeOf g t
  case ty of
    Sigma b -> lunbind b $ \((_, Embed t1), _) ->
      return t1
    _ -> error "projection is not a pair"
typeOf g (Second t) = do
  ty <- typeOf g t
  case ty of
    Sigma b -> lunbind b $ \((x, _), t2) -> do
      return (subst x (First t) t2)
    _ -> error "projection is not a pair"
typeOf g (InL l ty) = do
  case ty of
    s@(Sum t1 _) -> do
      checkType g s
      lTy <- typeOf g l
      if beq lTy t1
        then return ty
        else error "in annotation has wrong type"
    _ -> error "in annotation is not sum"
typeOf g (InR r ty) = do
  case ty of
    s@(Sum _ t2) -> do
      checkType g s
      rTy <- typeOf g r
      if beq rTy t2
        then return ty
        else error "in annotation has wrong type"
    _ -> error "in annotation is not sum"
typeOf g (Sum t1 t2) = do
  checkType g t1
  checkType g t2
  return Type
typeOf g (Case s b1 b2) = lunbind b1 $ \(x, t) -> lunbind b2 $ \(y, u) -> do
  ty <- typeOf g s
  case ty of
    Sum ty1 ty2 -> do
      tTy <- typeOf (addType x ty1 g) t
      uTy <- typeOf (addType y ty2 g) u
      if beq tTy uTy
        then return tTy -- FIXME : which one should we return?
        else error "case branch types are different"
    _ -> error "case is not sum"
typeOf g Unit = return UnitTy
typeOf g UnitTy = return Type

beq :: Term -> Term -> Bool
beq t1 t2 = aeq (runLFreshM $ nf t1) (runLFreshM $ nf t2)

nf :: Term -> LFreshM Term
nf (App t1 t2) = do
  t1' <- nf t1
  t2' <- nf t2
  case t1' of
    Lam b -> lunbind b $ \((x, _), tm) ->
      return $ subst x t2' tm -- FIXME: CBN?
    _ -> return $ App t1' t2'
nf (First p) = do
  p' <- nf p
  case p of
    TypedPair _ t _ -> return t
    _ -> return $ First p'
nf (Case s b1 b2) = do
  s' <- nf s
  lunbind b1 $ \(x, t) -> lunbind b2 $ \(y, u) ->
    case s' of
      InL s'' _ -> return $ subst x s'' t
      InR s'' _ -> return $ subst y s'' u
      _ -> return $ Case s' b1 b2 -- FIXME: b1' b2' required?
-- FIXME: Do we need eta-reduction?
nf t = return t