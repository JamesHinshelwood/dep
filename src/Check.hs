module Check where

import Control.Applicative
import Data.Maybe
import Debug.Trace
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
               | AppRhsErr TypeError
               | AppLhsNotLambda Term
               | AppLhsErr TypeError
               | PiBadAbstraction (Sort, Sort)
               | PiRhsType Term
               | PiLhsType Term
               | LetType TypeError
               | DeclType TypeError
               | PairType TypeError
               | ProductNotStar Term
               | ProductType TypeError
               | ProjectNotProduct Term
               | ProjectType TypeError
               | InWrongType Term
               | InType TypeError
               | SumNotStar Term
               | SumType TypeError
               | CaseBranchTypes Term Term
               | CaseNotSum Term
               | CaseType TypeError
               | FoldRecType Term
               | FoldNotRec Term
               | UnfoldRecType Term
               | UnfoldNotRec Term
               deriving (Show)

left :: (a -> b) -> Either a c -> Either b c
left f (Left x) = Left (f x)
left _ (Right x) = Right x

rules :: [(Sort, Sort)]
rules = [(Star, Star), (Star, Box), (Box, Star), (Box, Box)]

typeOf :: Context -> Term -> Either TypeError Term
typeOf _ (Srt Star) = Right (Srt Box)
typeOf _ (Srt Box) = Left BoxUntyped
typeOf g (Var x) = maybe (Left $ VarUnbound x) Right $ lookupType x g

typeOf g (Lam x ty tm) = left LambdaErr $ do
  typeOf g ty
  tmTy <- typeOf (addType x (nf g ty) g) tm
  return $ Pi x ty tmTy
typeOf g (App lhs rhs) = let lhsTy = typeOf g lhs in
                         case lhsTy of
                          Right (Pi x lTy rTy) -> case typeOf g rhs of
                                                  Right rhsTy ->
                                                    if betaEq g lTy rhsTy
                                                      then Right $ subst rTy x rhs
                                                      else Left $ AppRhsType lTy rhsTy
                                                  Left err -> Left $ AppRhsErr err
                          Right ty -> Left $ AppLhsNotLambda ty
                          Left err -> Left $ AppLhsErr err
typeOf g (Pi x lTy rTy) = case typeOf g lTy of
                            Right (Srt lSort) ->
                              case typeOf (addType x lTy g) rTy of
                                Right (Srt rSort) ->
                                  if (lSort, rSort) `elem` rules
                                    then Right $ Srt rSort
                                    else Left $ PiBadAbstraction (lSort, rSort)
                                _ -> Left $ PiRhsType rTy
                            _ -> Left $ PiLhsType lTy
typeOf g (Let x tm t) = case typeOf g tm of
                          Right ty -> typeOf (addType x ty g) (subst t x (nf g tm))
                          Left err -> Left $ LetType err
typeOf g (Decl x ty t) = case typeOf g ty of
                          Right _ -> typeOf (addType x ty g) t
                          Left err -> Left $ DeclType err
typeOf g (Pair t1 t2) = case typeOf g t1 of
                          Right ty1 -> case typeOf g t2 of
                            Right ty2 -> Right $ Product ty1 ty2
                            Left err -> Left $ PairType err
                          Left err -> Left $ PairType err
typeOf g (Product t1 t2) = case typeOf g t1 of
                            Right (Srt Star) -> case typeOf g t2 of
                              Right (Srt Star) -> Right $ Srt Star
                              Right ty -> Left $ ProductNotStar ty
                              Left err -> Left $ ProductType err
                            Right ty -> Left $ ProductNotStar ty
                            Left err -> Left $ ProductType err
typeOf g (First t) = case typeOf g t of
                        Right (Product t1 _) -> Right t1
                        Right ty -> Left $ ProjectNotProduct ty
                        Left err -> Left $ ProjectType err
typeOf g (Second t) = case typeOf g t of
                        Right (Product _ t2) -> Right t2
                        Right ty -> Left $ ProjectNotProduct ty
                        Left err -> Left $ ProjectType err
typeOf g (InL l ty) = case (nf g ty) of
                        s@(Sum t1 _) -> case typeOf g s of
                          Right _ -> case typeOf g l of
                            Right lTy | lTy == t1 -> Right s
                            Right lTy -> Left $ InWrongType ty
                            Left err -> Left $ InType err
                          Left err -> Left $ InType err
                        t -> Left $ InWrongType t
typeOf g (InR l ty) = case (nf g ty) of
                        s@(Sum _ t2) -> case typeOf g s of
                          Right _ -> case typeOf g l of
                            Right rTy | rTy == t2 -> Right s
                            Right ty -> Left $ InWrongType ty
                            Left err -> Left $ InType err
                          Left err -> Left $ InType err
                        t -> Left $ InWrongType t
typeOf g (Sum t1 t2) = case typeOf g t1 of
                        Right (Srt Star) -> case typeOf g t2 of
                          Right (Srt Star) -> Right $ Srt Star
                          Right ty -> Left $ SumNotStar ty
                          Left err -> Left $ SumType err
                        Right ty -> Left $ SumNotStar ty
                        Left err -> Left $ SumType err
typeOf g (Case s x1 t1 x2 t2) = case typeOf g s of
                                  Right (Sum ty1 ty2) -> do
                                    t <- typeOf (addType x1 ty1 g) t1
                                    t' <- typeOf (addType x2 ty2 g) t2
                                    if t == t' then Right t
                                    else Left $ CaseBranchTypes t t'
                                  Right ty -> Left $ CaseNotSum ty
                                  Left err -> Left $ CaseType err
typeOf g (Fold u t) = do
  tTy <- typeOf g t
  let u' = nf g u
  case u' of
    (Rec x ty) -> if tTy == subst ty x u'
      then return u
      else Left $ FoldRecType tTy
    ty -> Left $ FoldNotRec ty
typeOf g (Unfold u t) = do
  tTy <- typeOf g t
  let u' = nf g u
  case u' of
    (Rec x ty) -> if tTy == u'
      then Right (subst ty x u')
      else Left $ UnfoldRecType tTy
    ty -> Left $ UnfoldNotRec ty
typeOf g (Rec x t) = do
  typeOf (addType x (Srt Star) g) t -- FIXME: Unsure
  return $ Srt Star
  -- Check positive
  
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
alphaEq (Pair s1 t1) (Pair s2 t2) = alphaEq s1 s2 && alphaEq t1 t2
alphaEq (Product s1 t1) (Product s2 t2) = alphaEq s1 s2 && alphaEq t1 t2
alphaEq (First t1) (First t2) = alphaEq t1 t2
alphaEq (Second t1) (Second t2) = alphaEq t1 t2
alphaEq (InL tm1 ty1) (InL tm2 ty2) = alphaEq tm1 tm2 && alphaEq ty1 ty2
alphaEq (InR tm1 ty1) (InR tm2 ty2) = alphaEq tm1 tm2 && alphaEq ty1 ty2
alphaEq (Sum s1 t1) (Sum s2 t2) = alphaEq s1 s2 && alphaEq t1 t2
alphaEq (Case s1 x1 t1 y1 u1) (Case s2 x2 t2 y2 u2) = alphaEq s1 s2 && alphaEq t1 (subst t2 x2 (Var x1)) && alphaEq u1 (subst u2 y2 (Var y1))
alphaEq (Fold ty1 t1) (Fold ty2 t2) = alphaEq ty1 ty2 && alphaEq t1 t2
alphaEq (Unfold ty1 t1) (Unfold ty2 t2) = alphaEq ty1 ty2 && alphaEq t1 t2
alphaEq (Rec x1 t1) (Rec x2 t2) = alphaEq t1 (subst t2 x2 (Var x1))
alphaEq (Unit) (Unit) = True
alphaEq (UnitTy) (UnitTy) = True
alphaEq _ _ = False

nf :: Context -> Term -> Term
nf g s@(Srt _) = s
nf g (Var v) | isJust $ lookupTerm v g = nf g (fromJust $ lookupTerm v g) -- FIXME: Unused (terms are never in context, TermBinding should be removed)
nf g v@(Var _) = v
nf g (App (Lam x _ t1) t2) = nf g (subst t1 x t2)
nf g (App t1 t2) = nf g (App (nf g t1) (nf g t2))
nf g (Lam x ty tm) = Lam x (nf g ty) (nf g tm)
nf g (Pi x ty tm) = Pi x (nf g ty) (nf g tm)
nf g l@(Let _ _ _) = l -- FIXME: Not sure
nf g d@(Decl _ _ _) = d -- FIXME: Not sure
nf g (Pair t1 t2) = Pair (nf g t1) (nf g t2)
nf g (Product t1 t2) = Product (nf g t1) (nf g t2)
nf g (First (Pair t _)) = nf g t
nf g (Second (Pair _ t)) = nf g t
nf g (First t) = First (nf g t)
nf g (Second t) = Second (nf g t)
nf g (InL t ty) = InL (nf g t) ty
nf g (InR t ty) = InR (nf g t) ty
nf g (Sum t1 t2) = Sum (nf g t1) (nf g t2)
nf g (Case (InL s _) x t _ _) = nf g (subst t x s)
nf g (Case (InR s _) _ _ x t) = nf g (subst t x s)
nf g (Case s x1 t1 x2 t2) = Case (nf g s) x1 (nf g t1) x2 (nf g t2)
nf g (Unfold _ (Fold _ t)) = nf g t
nf g (Fold ty t) = Fold (nf g ty) (nf g t)
nf g (Unfold ty t) = Unfold (nf g ty) (nf g t)
nf g (Rec x t) = Rec x (nf g t)
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
subst (Pair t1 t2) x s = Pair (subst t1 x s) (subst t2 x s)
subst (Product t1 t2) x s = Product (subst t1 x s) (subst t2 x s)
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
subst (Fold ty t) y s = Fold (subst ty y s) (subst t y s)
subst (Unfold ty t) y s = Unfold (subst ty y s) (subst t y s)
subst (Rec x t) y s = if x == y
  then (Rec x t)
  else if x `notElem` (freeVars s)
    then (Rec x (subst t y s))
    else
      let newX = freshVar x (freeVars t ++ freeVars s) in
      let newTm = Rec newX (subst t x (Var newX)) in
      subst newTm y s
subst (Unit) _ _ = Unit
subst (UnitTy) _ _ = UnitTy

abst con x1 ty tm x2 nTm = if x1 == x2
  then con x1 (subst ty x2 nTm) tm
  else if x1 `notElem` (freeVars nTm)
    then con x1 (subst ty x2 nTm) (subst tm x2 nTm)
    else
      let newX1 = freshVar x1 (freeVars tm ++ freeVars nTm) in
      let newTm = con newX1 (subst ty x1 (Var newX1)) (subst tm x1 (Var newX1)) in
      subst newTm x2 nTm

whnf :: Context -> Term -> Term
whnf g t = case whred g t of
  Just t' -> whnf g t'
  Nothing -> t

whred :: Context -> Term -> Maybe Term
whred g (App t1 t2) | isJust $ whred g t1 = Just $ App (fromJust $ whred g t1) t2
whred _ (App (Lam x _ t1) t2) = Just $ subst t1 x t2
whred _ (First (Pair t1 _)) = Just $ t1
whred g (First t) | isJust $ whred g t = Just $ First $ fromJust $ whred g t
whred _ (Second (Pair _ t2)) = Just $ t2
whred g (Second t) | isJust $ whred g t = Just $ Second $ fromJust $ whred g t
whred g (Var v) = lookupTerm v g
whred _ _ = Nothing

freshVar :: Sym -> [Sym] -> Sym
freshVar x vars = if x `elem` vars then freshVar (x ++ "'") vars else x