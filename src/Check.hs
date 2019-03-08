module Check where

import Unbound.LocallyNameless hiding (Refl)
import Syntax

type Context = [(Name Term, Term)]

addType :: Name Term -> Term -> Context -> Context
addType x ty g = (x, ty) : g

allEqual :: [Term] -> Bool
allEqual [] = True
allEqual (x:xs) = all (beq x) xs

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
typeOf g (Variant lbl tm s) = do
  tmTy <- typeOf g tm
  s' <- nf s -- FIXME: Discuss this problem, the application might not be well typed. Is it even a problem?
  case s' of
    Sum ts -> case lookup lbl ts of
      Just ty -> if ty `beq` tmTy then return s' else error "variant has wrong type"
      Nothing -> error "invalid variant in sum"
    _ -> error "variant annotation is not sum"
typeOf g (Sum s) = do
  mapM (\(l, t) -> checkType g t) s
  return Type
typeOf g (Case s l) = do
  ty <- typeOf g s
  case ty of
    Sum tys -> do
      types <- mapM (\(lbl, b') -> lunbind b' $ \(x, t) -> case lookup lbl tys of
        Just ty' -> typeOf (addType x ty' g) t
        _ -> error "invalid variant in case branch") l
      if allEqual types
        then return $ head types
        else error "case branches have different types"
    _ -> error "case type is not a sum"
typeOf _ Unit = return UnitTy
typeOf _ UnitTy = return Type
typeOf g (Eq a b ty) = do
  checkType g ty
  aTy <- typeOf g a
  bTy <- typeOf g b
  if beq aTy ty && beq bTy ty
    then return Type
    else error "equal type is not well formed"
typeOf _ (Refl ann) = do
  case ann of
    Eq a b _ -> if beq a b -- FIXME: Check a : T and b : T
      then return ann
      else error "refl on non-equal terms"
    _ -> error "refl has bad annotation"
typeOf g (Split c p b) = lunbind b $ \(z, t) -> do
  pTy <- typeOf g p
  case pTy of
    Eq m n a -> do
      cTy <- typeOf g c
      case cTy of
        Pi b' ->
          lunbind b' $ \((x, Embed a'), Pi b'') ->
          lunbind b'' $ \((y, Embed a''), Pi b''') ->
          lunbind b''' $ \((_, Embed (Eq (Var x') (Var y') a''')), _) ->
          if x == x' && y == y' && beq a a' && beq a a'' && beq a a'''
            then do
              checkType g cTy
              tTy <- typeOf (addType z a g) t
              if tTy `beq` (App (App (App c (Var z)) (Var z)) (Refl (Eq (Var z) (Var z) a)))
                then do
                  ret <- nf (App (App (App c m) n) p) -- FIXME: nf needed?
                  return ret
                else error "case t is badly formed"
            else error "case c is badly formed"
        _ -> error "case c is badly formed"
    _ -> error "case p must be an equality type"


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
nf (Case v ts) = do
  v' <- nf v
  case v' of
    Variant lbl t _ -> case lookup lbl ts of
      Just b -> lunbind b $ \(x, tm) -> return $ subst x t tm
      _ -> return $ Case v' ts
    _ -> return $ Case v' ts
{-nf (Case s b1 b2) = do
  s' <- nf s
  lunbind b1 $ \(x, t) -> lunbind b2 $ \(y, u) ->
    case s' of
      InL s'' _ -> return $ subst x s'' t
      InR s'' _ -> return $ subst y s'' u
      _ -> return $ Case s' b1 b2 -- FIXME: b1' b2'?-}
nf (Split _ (Refl (Eq x _ _)) b) = lunbind b $ \(_, t) -> do
  return (App t x)
-- FIXME: Do we need eta-reduction?
nf t = return t