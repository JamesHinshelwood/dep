module Check where

import Data.Maybe
import Data.List
import Syntax

data Binding = TypeBinding Sym Term
             | TermBinding Sym Term

type Context = [Binding]

lookupType :: Sym -> Context -> Maybe Term
lookupType x [] = Nothing
lookupType x ((TypeBinding y ty) : g) | x == y = Just ty
lookupType x (_ : g) = lookupType x g

lookupTerm :: Sym -> Context -> Maybe Term
lookupTerm x [] = Nothing
lookupTerm x ((TermBinding y tm) : g) | x == y = Just tm
lookupTerm x (_ : g) = lookupTerm x g

addType :: Sym -> Term -> Context -> Context
addType x ty g = (TypeBinding x ty) : g

addTerm :: Sym -> Term -> Context -> Context
addTerm x tm g = (TermBinding x tm) : g

data TypeError = BoxUntyped
               | VarUnbound Sym
               | LambdaInner TypeError
               | LambdaType TypeError
               | AppRhsType Term Term
               | AppRhsErr TypeError
               | AppLhsNotLambda Term
               | AppLhsErr TypeError
               | PiBadAbstraction (Sort, Sort)
               | PiRhsType Term
               | PiLhsType Term
               | LetType TypeError
               | DeclType TypeError
               deriving (Show)

rules :: [(Sort, Sort)]
rules = [(Star, Star), (Star, Box), (Box, Star), (Box, Box)]

typeOf :: Context -> Term -> Either TypeError Term
typeOf _ (Srt Star) = Right (Srt Box)
typeOf _ (Srt Box) = Left BoxUntyped
typeOf g (Var x) = case lookupType x g of
                          Just ty -> Right ty
                          Nothing ->
                            case lookupTerm x g of
                              Just tm -> Right tm
                              Nothing -> Left $ VarUnbound x
typeOf g (Lam x ty tm) = case typeOf g ty of
                          Right _ -> case typeOf (addType x ty g) tm of
                                      Right tmTy -> Right $ Pi x ty tmTy
                                      Left err -> Left $ LambdaInner err
                          Left err -> Left $ LambdaType err
typeOf g (App lhs rhs) = let lhsTy = typeOf g lhs in
                         case lhsTy of
                          Right (Pi x lTy rTy) -> case typeOf g rhs of
                                                  Right rhsTy ->
                                                    if typeEq g lTy rhsTy
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
                          Right ty -> typeOf (addType x ty (addTerm x tm g)) t
                          Left err -> Left $ LetType err
typeOf g (Decl x ty t) = case typeOf g ty of
                          Right _ -> typeOf (addType x ty g) t
                          Left err -> Left $ DeclType err

typeEq :: Context -> Term -> Term -> Bool
typeEq g t1 t2 = whEq g (whnf g t1) (whnf g t2)

whEq :: Context -> Term -> Term -> Bool
whEq _ (Srt s1) (Srt s2) = s1 == s2
whEq _ (Var v1) (Var v2) = v1 == v2
whEq g (Lam x1 ty1 tm1) (Lam x2 ty2 tm2) = typeEq g ty1 ty2 && typeEq (addType x1 ty2 g) tm1 tm2
whEq g (Pi x1 lTy1 rTy1) (Pi x2 lTy2 rTy2) = typeEq g lTy1 lTy2 && typeEq (addType x1 lTy2 g) rTy1 rTy2
whEq g (App lhs1 rhs1) (App lhs2 rhs2) = typeEq g lhs1 lhs2 && typeEq g rhs1 rhs2
whEq g s (Lam x ty tm) = typeEq (addType x ty g) (App s (Var x)) tm
whEq g (Lam x ty tm) t = typeEq (addType x ty g) tm (App t (Var x))
whEq _ _ _ = False

subst :: Term -> Sym -> Term -> Term
subst s@(Srt _) _ _ = s
subst v@(Var x1) x2 tm = if x1 == x2 then tm else v
subst (Lam x1 ty tm) x2 nTm = if x1 == x2 then Lam x1 (subst ty x2 nTm) tm else
                                if x1 `notElem` (freeVars nTm) then Lam x1 (subst ty x2 nTm) (subst tm x2 nTm) else
                                  let newX1 = freshVar x1 (freeVars tm ++ freeVars nTm) in
                                  let newTm = Lam newX1 (subst ty x1 (Var newX1)) (subst tm x1 (Var newX1)) in
                                  subst newTm x2 nTm
subst (App lhs rhs) x tm = App (subst lhs x tm) (subst rhs x tm)
subst (Pi x1 ty tm) x2 nTm = if x1 == x2 then Pi x1 (subst ty x2 nTm) tm else
                                if x1 `notElem` (freeVars nTm) then Pi x1 (subst ty x2 nTm) (subst tm x2 nTm) else
                                  let newX1 = freshVar x1 (freeVars tm ++ freeVars nTm) in
                                  let newTm = Pi newX1 (subst ty x1 (Var newX1)) (subst tm x1 (Var newX1)) in
                                  subst newTm x2 nTm

whnf :: Context -> Term -> Term
whnf g t = case whred g t of
  Just t -> whnf g t
  Nothing -> t

whred :: Context -> Term -> Maybe Term
whred g (App t1 t2) | isJust $ whred g t1 = Just $ App (fromJust $ whred g t1) t2
whred g (App (Lam x ty t1) t2) = Just $ subst t1 x t2
whred g (Var v) = lookupTerm v g
whred _ _ = Nothing

freshVar :: Sym -> [Sym] -> Sym
freshVar x vars = if x `elem` vars then freshVar (x ++ "'") vars else x