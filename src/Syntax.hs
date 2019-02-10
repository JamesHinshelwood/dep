module Syntax where

import Data.List

type Sym = String

data Term
  = Srt Sort
  | Var Sym
  | Lam Sym Term Term
  | App Term Term
  | Pi Sym Term Term
  | Let Sym Term Term
  | Decl Sym Term Term
  | Pair Term Term
  | Product Term Term
  | First Term
  | Second Term
  | InL Term Term
  | InR Term Term
  | Sum Term Term
  | Case Term Sym Term Sym Term
  | Fold Term Term
  | Unfold Term Term
  | Rec Sym Term
  deriving (Eq)

instance Show Term where
  show (Srt s) = show s
  show (Var v) = v
  show (Lam x t1 t2) = "(\\" ++ x ++ " : " ++ show t1 ++ ". " ++ show t2 ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Pi x t1 t2) = if x `elem` (freeVars t2)
                        then "(" ++ x ++ " : " ++ show t1 ++ ") -> " ++ show t2
                        else show t1 ++ " -> " ++ show t2
  show (Let _ _ t) = show t
  show (Decl _ _ t) = show t
  show (Pair t1 t2) = "(" ++ show t1 ++ " , " ++ show t2 ++ ")"
  show (Product t1 t2) = "(" ++ show t1 ++ " × " ++ show t2 ++ ")"
  show (First t) = show t ++ ".1"
  show (Second t) = show t ++ ".2"
  show (InL t ty) = "inl " ++ show t ++ " : " ++ show ty
  show (InR t ty) = "inr " ++ show t ++ " : " ++ show ty
  show (Sum t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++ ")"
  show (Case s x1 t1 x2 t2) = "case " ++ show s ++ " of inl " ++ x1 ++ " -> " ++ show t1 ++ " | " ++ "inr " ++ x2 ++ " -> " ++ show t2
  show (Fold ty t) = "fold " ++ "[" ++ show ty ++ "] " ++ show t
  show (Unfold ty t) = "unfold " ++ "[" ++ show ty ++ "] " ++ show t
  show (Rec x t) = "~" ++ x ++ ". " ++ show t

freeVars :: Term -> [Sym]
freeVars (Srt _) = []
freeVars (Var x) = [x]
freeVars (Lam x ty tm) = ((freeVars tm) \\ [x]) `union` (freeVars ty)
freeVars (App lhs rhs) = (freeVars lhs) `union` (freeVars rhs)
freeVars (Pi x lTy rTy) = ((freeVars rTy) \\ [x]) `union` (freeVars lTy)
freeVars (Let x t1 t2) = ((freeVars t2) \\ [x]) `union` (freeVars t1)
freeVars (Decl x t1 t2) = ((freeVars t2) \\ [x]) `union` (freeVars t1)
freeVars (Pair t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Product t1 t2) = freeVars t1 `union` freeVars t2
freeVars (First t) = freeVars t
freeVars (Second t) = freeVars t
freeVars (InL t ty) = freeVars t `union` freeVars ty
freeVars (InR t ty) = freeVars t `union` freeVars ty
freeVars (Sum t1 t2) = freeVars t1 `union` freeVars t2
freeVars (Case s x1 t1 x2 t2) = freeVars s `union` ((freeVars t1) \\ [x1]) `union` ((freeVars t2) \\ [x2])
freeVars (Fold ty t) = freeVars ty `union` freeVars t
freeVars (Unfold ty t) = freeVars ty `union` freeVars t
freeVars (Rec x t) = freeVars t \\ [x]

data Sort
  = Star
  | Box
  deriving (Eq)

instance Show Sort where
  show Star = "*"
  show Box = "[]"