module Check where

import Data.Maybe
import Data.List
import Syntax

type Context = [(Sym, Term)]
type Bindings = [(Sym, Term)]

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
               | DeclType TypeError
               | BindType TypeError
               deriving (Show)

rules :: [(Sort, Sort)]
rules = [(Star, Star), (Star, Box), (Box, Star), (Box, Box)]

typeOf :: Context -> Bindings -> Prog -> Either TypeError Term
typeOf g b (Tm term) = typeOfTerm g b term
typeOf g b (DSeq (x, ty) p) = case typeOfTerm g b ty of
                                Right _ -> typeOf ((x, ty) : g) b p
                                Left err -> Left $ DeclType err
typeOf g b (BSeq (x, tm) p) = case typeOfTerm g b tm of
                                Right ty -> typeOf ((x, ty) : g) ((x, tm) : b) p
                                Left err -> Left $ BindType err

typeOfTerm :: Context -> Bindings -> Term -> Either TypeError Term
typeOfTerm _ _ (Srt Star) = Right (Srt Box)
typeOfTerm _ _ (Srt Box) = Left BoxUntyped
typeOfTerm g b (Var x) = case lookup x g of
                          Just ty -> Right ty
                          Nothing ->
                            case lookup x b of
                              Just tm -> Right tm
                              Nothing -> Left $ VarUnbound x
typeOfTerm g b (Lam x ty tm) = case typeOfTerm g b ty of
                          Right _ -> case typeOfTerm ((x, ty) : g) b tm of
                                      Right tmTy -> Right $ Pi x ty tmTy
                                      Left err -> Left $ LambdaInner err
                          Left err -> Left $ LambdaType err
typeOfTerm g b (App lhs rhs) = let lhsTy = typeOfTerm g b lhs in
                         case lhsTy of
                          Right (Pi x lTy rTy) -> case typeOfTerm g b rhs of
                                                  Right rhsTy ->
                                                    if typeEq g b lTy rhsTy
                                                      then Right $ subst rTy x rhs
                                                      else Left $ AppRhsType lTy rhsTy
                                                  Left err -> Left $ AppRhsErr err
                          Right ty -> Left $ AppLhsNotLambda ty
                          Left err -> Left $ AppLhsErr err
typeOfTerm g b (Pi x lTy rTy) = case typeOfTerm g b lTy of
                            Right (Srt lSort) ->
                              case typeOfTerm ((x, lTy) : g) b rTy of
                                Right (Srt rSort) ->
                                  if (lSort, rSort) `elem` rules
                                    then Right $ Srt rSort
                                    else Left $ PiBadAbstraction (lSort, rSort)
                                _ -> Left $ PiRhsType rTy
                            _ -> Left $ PiLhsType lTy

typeEq :: Context -> Bindings -> Term -> Term -> Bool
typeEq g b t1 t2 = whEq g b (whnf b t1) (whnf b t2)

whEq :: Context -> Bindings -> Term -> Term -> Bool
whEq _ _ (Srt s1) (Srt s2) = s1 == s2
whEq _ _ (Var v1) (Var v2) = v1 == v2
whEq g b (Lam x1 ty1 tm1) (Lam x2 ty2 tm2) = typeEq g b ty1 ty2 && typeEq ((x1, ty2) : g) b tm1 tm2
whEq g b (Pi x1 lTy1 rTy1) (Pi x2 lTy2 rTy2) = typeEq g b lTy1 lTy2 && typeEq ((x1, lTy2) : g) b rTy1 rTy2
whEq g b (App lhs1 rhs1) (App lhs2 rhs2) = typeEq g b lhs1 lhs2 && typeEq g b rhs1 rhs2
whEq g b s (Lam x ty tm) = typeEq ((x, ty) : g) b (App s (Var x)) tm
whEq g b (Lam x ty tm) t = typeEq ((x, ty) : g) b tm (App t (Var x))
whEq _ _ _ _ = False

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

whnf :: Bindings -> Term -> Term
whnf b t = case whred b t of
  Just t -> whnf b t
  Nothing -> t

whred :: Context -> Term -> Maybe Term
whred b (App t1 t2) | isJust $ whred b t1 = Just $ App (fromJust $ whred b t1) t2
whred b (App (Lam x ty t1) t2) = Just $ subst t1 x t2
whred b (Var v) = lookup v b
whred _ _ = Nothing

freshVar :: Sym -> [Sym] -> Sym
freshVar x vars = if x `elem` vars then freshVar (x ++ "'") vars else x