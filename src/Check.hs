module Check where

import Control.Applicative
import Data.Maybe
import Syntax

data Binding = TypeBinding Sym Term
             | TermBinding Sym Term
             deriving (Show)

type Context = [Binding]

lookupType :: Sym -> Context -> Maybe Term
lookupType _ [] = Nothing
lookupType x ((TypeBinding y ty) : _) | x == y = Just ty
lookupType x (_ : g) = lookupType x g

lookupTerm :: Sym -> Context -> Maybe Term
lookupTerm _ [] = Nothing
lookupTerm x ((TermBinding y tm) : _) | x == y = Just tm
lookupTerm x (_ : g) = lookupTerm x g

addType :: Sym -> Term -> Context -> Context
addType x ty g = (TypeBinding x ty) : g

addTerm :: Sym -> Term -> Context -> Context
addTerm x tm g = (TermBinding x tm) : g

data TypeError = BoxUntyped
               | VarUnbound Sym
               | LambdaErr TypeError
               | AppRhsType Term Term
               | AppLhsNotLambda Term
               | AppErr TypeError
               | PiBadAbstraction (Sort, Sort)
               | PiRhsType Term
               | PiLhsType Term
               | PiErr TypeError
               | LetErr TypeError
               | DeclErr TypeError
               | TypedPairErr TypeError
               | TypedPairMismatch Term Term
               | TypedPairNotSigma Term
               | SigmaNotStar Term
               | SigmaErr TypeError
               | ProjectNotSigma Term
               | ProjectErr TypeError
               | InWrongType Term
               | InErr TypeError
               | SumNotStar Term
               | SumErr TypeError
               | CaseBranchTypes Term Term
               | CaseNotSum Term
               | CaseErr TypeError
               deriving (Show)

left :: (a -> b) -> Either a c -> Either b c
left f (Left x) = Left (f x)
left _ (Right x) = Right x

throw :: a -> Either a b
throw = Left

rules :: [(Sort, Sort)]
rules = [(Star, Star), (Star, Box), (Box, Star), (Box, Box)]

typeOf :: Context -> Term -> Either TypeError Term
typeOf _ (Srt Star) = Right (Srt Box)

typeOf _ (Srt Box) = Left BoxUntyped

typeOf g (Var x) = maybe (Left $ VarUnbound x) Right $ lookupType x g

typeOf g (Lam x ty tm) = left LambdaErr $ do
  typeOf g ty
  tmTy <- typeOf (addType x ty g) tm
  return $ Pi x ty tmTy

typeOf g (App lhs rhs) = left AppErr $ do
  lhsTy <- typeOf g lhs
  rhsTy <- typeOf g rhs
  case lhsTy of
    Pi x lTy rTy -> if betaEq g lTy rhsTy
      then return $ subst rTy x rhs
      else throw $ AppRhsType lTy rhsTy
    ty -> throw $ AppLhsNotLambda ty

typeOf g (Pi x lTy rTy) = left PiErr $ do
  lSort <- typeOf g lTy
  rSort <- typeOf (addType x lTy g) rTy
  case (lSort, rSort) of
    (Srt l, Srt r) -> if (l, r) `elem` rules
      then return $ Srt r
      else throw $ PiBadAbstraction (l, r)
    (Srt l, _) -> throw $ PiRhsType rTy
    (_, _) -> throw $ PiLhsType lTy

typeOf g (Let x tm t) = left LetErr $ do
  ty <- typeOf g tm
  letTy <- typeOf (addType x ty g) (subst t x tm)
  return letTy

typeOf g (Decl x ty t) = left DeclErr $ do
  typeOf g ty
  declTy <- typeOf (addType x ty g) t
  return declTy

typeOf g (TypedPair t1 t2 ty) = left TypedPairErr $ do
  case ty of
    Sigma x s t -> do
      t1Ty <- typeOf g t1
      t2Ty <- typeOf g t2
      if betaEq g t1Ty s
        then if betaEq g t2Ty (subst t x t1)
          then return ty
          else throw $ TypedPairMismatch t2Ty (subst t x t1)
        else throw $ TypedPairMismatch t1Ty s
    ty -> throw $ TypedPairNotSigma ty

typeOf g (Sigma x s t) = left SigmaErr $ do
  sKind <- typeOf g s
  tKind <- typeOf (addType x s g) t
  case (sKind, tKind) of
    (Srt Star, Srt Star) -> return $ Srt Star
    (Srt Star, ty) -> throw $ SigmaNotStar ty
    (ty, _) -> throw $ SigmaNotStar ty

typeOf g (First t) = left ProjectErr $ do
  ty <- typeOf g t
  case ty of
    Sigma _ t1 _ -> return t1
    ty -> throw $ ProjectNotSigma ty

typeOf g (Second t) = left ProjectErr $ do
  ty <- typeOf g t
  case ty of
    Sigma x _ t2 -> return (subst t2 x (First t))
    ty -> throw $ ProjectNotSigma ty

typeOf g (InL l ty) = left InErr $ do
  let ty' = nf g ty
  case ty' of
    s@(Sum t1 _) -> do
      typeOf g s
      lTy <- typeOf g l
      if lTy == t1
        then return s
        else throw $ InWrongType lTy
    ty -> throw $ InWrongType ty

typeOf g (InR r ty) = left InErr $ do
  let ty' = nf g ty
  case ty' of
    s@(Sum _ t2) -> do
      typeOf g s
      rTy <- typeOf g r
      if rTy == t2
        then return s
        else throw $ InWrongType rTy
    ty -> throw $ InWrongType ty

typeOf g (Sum t1 t2) = left SumErr $ do
  ty1 <- typeOf g t1
  ty2 <- typeOf g t2
  case (ty1, ty2) of
    (Srt Star, Srt Star) -> return $ Srt Star
    (Srt Star, ty) -> throw $ SumNotStar ty
    (ty, _) -> throw $ SumNotStar ty

typeOf g (Case s x1 t1 x2 t2) = left CaseErr $ do
  ty <- typeOf g s
  case ty of
    Sum ty1 ty2 -> do
      t <- typeOf (addType x1 ty1 g) t1
      t' <- typeOf (addType x2 ty2 g) t2
      if t == t'
        then return t
        else throw $ CaseBranchTypes t t'
    ty -> throw $ CaseNotSum ty

typeOf g (Unit) = Right UnitTy

typeOf g (UnitTy) = Right $ Srt Star

betaEq :: Context -> Term -> Term -> Bool
betaEq g t1 t2 = alphaEq (nf g t1) (nf g t2)

alphaEq :: Term -> Term -> Bool
alphaEq (Srt s1) (Srt s2) = s1 == s2
alphaEq (Var v1) (Var v2) = v1 == v2
alphaEq (Lam x1 ty1 tm1) (Lam x2 ty2 tm2) = alphaEq ty1 ty2 && alphaEq tm1 (subst tm2 x2 (Var x1))
alphaEq (App s1 t1) (App s2 t2) = alphaEq s1 s2 && alphaEq t1 t2
alphaEq (Pi x1 ty1 tm1) (Pi x2 ty2 tm2) = alphaEq ty1 ty2 && alphaEq tm1 (subst tm2 x2 (Var x1))
alphaEq (Let x1 s1 t1) (Let x2 s2 t2) = alphaEq s1 s2 && alphaEq t1 (subst t2 x2 (Var x1))
alphaEq (Decl x1 s1 t1) (Decl x2 s2 t2) = alphaEq s1 s2 && alphaEq t1 (subst t2 x2 (Var x1))
alphaEq (TypedPair s1 t1 ty1) (TypedPair s2 t2 ty2) = alphaEq s1 s2 && alphaEq t1 t2 && alphaEq ty1 ty2
alphaEq (Sigma x1 s1 t1) (Sigma x2 s2 t2) = alphaEq s1 s2 && alphaEq t1 (subst t2 x2 (Var x1))
alphaEq (First t1) (First t2) = alphaEq t1 t2
alphaEq (Second t1) (Second t2) = alphaEq t1 t2
alphaEq (InL tm1 ty1) (InL tm2 ty2) = alphaEq tm1 tm2 && alphaEq ty1 ty2
alphaEq (InR tm1 ty1) (InR tm2 ty2) = alphaEq tm1 tm2 && alphaEq ty1 ty2
alphaEq (Sum s1 t1) (Sum s2 t2) = alphaEq s1 s2 && alphaEq t1 t2
alphaEq (Case s1 x1 t1 y1 u1) (Case s2 x2 t2 y2 u2) = alphaEq s1 s2 && alphaEq t1 (subst t2 x2 (Var x1)) && alphaEq u1 (subst u2 y2 (Var y1))
alphaEq (Unit) (Unit) = True
alphaEq (UnitTy) (UnitTy) = True
alphaEq _ _ = False

nf :: Context -> Term -> Term
nf g s@(Srt _) = s
nf g (Var v) | isJust $ lookupTerm v g = nf g (fromJust $ lookupTerm v g)
nf g v@(Var _) = v
nf g (App (Lam x _ t1) t2) = nf g (subst t1 x t2)
nf g (App t1 t2) = App (nf g t1) (nf g t2)
nf g (Lam x ty tm) = Lam x (nf g ty) (nf g tm)
nf g (Pi x ty tm) = Pi x (nf g ty) (nf g tm)
nf g l@(Let _ _ _) = l -- FIXME: Not sure
nf g d@(Decl _ _ _) = d -- FIXME: Not sure
nf g (TypedPair t1 t2 ty) = TypedPair (nf g t1) (nf g t2) (nf g ty)
nf g (Sigma x t1 t2) = Sigma x (nf g t1) (nf g t2)
nf g (First (TypedPair t _ _)) = nf g t
nf g (Second (TypedPair _ t _)) = nf g t
nf g (First t) = First (nf g t)
nf g (Second t) = Second (nf g t)
nf g (InL t ty) = InL (nf g t) ty
nf g (InR t ty) = InR (nf g t) ty
nf g (Sum t1 t2) = Sum (nf g t1) (nf g t2)
nf g (Case (InL s _) x t _ _) = nf g (subst t x s)
nf g (Case (InR s _) _ _ x t) = nf g (subst t x s)
nf g (Case s x1 t1 x2 t2) = Case (nf g s) x1 (nf g t1) x2 (nf g t2)
nf g (Unit) = Unit
nf g (UnitTy) = UnitTy

subst :: Term -> Sym -> Term -> Term
subst s@(Srt _) _ _ = s
subst v@(Var x1) x2 tm = if x1 == x2 then tm else v
subst (Lam x1 ty tm) x2 nTm = abst Lam x1 ty tm x2 nTm
subst (App lhs rhs) x tm = App (subst lhs x tm) (subst rhs x tm)
subst (Pi x1 ty tm) x2 nTm = abst Pi x1 ty tm x2 nTm
subst (Let x t1 t2) y s = if x == y
  then Let x (subst t1 y s) t2
  else if x `notElem` (freeVars s)
    then Let x (subst t1 y s) (subst t2 y s)
    else
      let newX = freshVar x (freeVars s ++ freeVars t1 ++ freeVars t2) in
      let newTm = Let newX t1 (subst t2 x (Var newX)) in
      subst newTm y s
subst (Decl x t1 t2) y s = if x == y
 then Decl x (subst t1 y s) t2
 else if x `notElem` (freeVars s)
   then Decl x (subst t1 y s) (subst t2 y s)
   else
     let newX = freshVar x (freeVars s ++ freeVars t1 ++ freeVars t2) in
     let newTm = Decl newX t1 (subst t2 x (Var newX)) in
     subst newTm y s
subst (TypedPair t1 t2 ty) x s = TypedPair (subst t1 x s) (subst t2 x s) (subst ty x s)
subst (Sigma x t1 t2) y s = if x == y
  then Sigma x (subst t1 y s) t2
  else if x `notElem` (freeVars s)
    then Sigma x (subst t1 y s) (subst t2 y s)
    else
      let newX = freshVar x (freeVars t1 ++ freeVars t2 ++ freeVars s) in
      let newTm = Sigma newX (subst t1 x (Var newX)) (subst t2 x (Var newX)) in
      subst newTm y s
subst (First t) x s = First (subst t x s)
subst (Second t) x s = Second (subst t x s)
subst (InL t ty) x s = InL (subst t x s) (subst ty x s)
subst (InR t ty) x s = InR (subst t x s) (subst ty x s)
subst (Sum t1 t2) x s = Sum (subst t1 x s) (subst t2 x s)
subst (Case u x1 t1 x2 t2) y s = Case (subst u y s) x1' (subst t1' y s) x2' (subst t2' y s)
  where (x1', t1') = if y == x1
          then (x1, t1)
          else if x1 `notElem` (freeVars s)
            then (x1, t1)
            else
              let newX1 = freshVar x1 (freeVars u ++ freeVars s ++ freeVars t1 ++ freeVars t2) in
              let newT1 = subst t1 x1 (Var newX1) in
              (newX1, newT1)
        (x2', t2') = if y == x2
          then (x2, t2)
          else if x2 `notElem` (freeVars s)
            then (x2, t2)
            else
              let newX2 = freshVar x2 (freeVars u ++ freeVars s ++ freeVars t1 ++ freeVars t2) in
              let newT2 = subst t2 x2 (Var newX2) in
              (newX2, newT2)
subst (Unit) y s = Unit
subst (UnitTy) y s = UnitTy

abst con x1 ty tm x2 nTm = if x1 == x2
  then con x1 (subst ty x2 nTm) tm
  else if x1 `notElem` (freeVars nTm)
    then con x1 (subst ty x2 nTm) (subst tm x2 nTm)
    else
      let newX1 = freshVar x1 (freeVars tm ++ freeVars nTm) in
      let newTm = con newX1 (subst ty x1 (Var newX1)) (subst tm x1 (Var newX1)) in
      subst newTm x2 nTm

freshVar :: Sym -> [Sym] -> Sym
freshVar x vars = if x `elem` vars then freshVar (x ++ "'") vars else x