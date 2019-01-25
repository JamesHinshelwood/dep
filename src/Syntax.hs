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
  deriving (Eq)

instance Show Term where
  show (Srt s) = show s
  show (Var v) = v
  show (Lam x t1 t2) = "\\" ++ x ++ " : " ++ show t1 ++ ". " ++ show t2
  show (App t1@(Lam _ _ _) t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (App t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"
  show (Pi x t1 t2) = if x `elem` (freeVars t2)
                        then "(" ++ x ++ " : " ++ show t1 ++ ") -> " ++ show t2
                        else show t1 ++ " -> " ++ show t2
  show (Let x t1 t2) = show t2
  show (Decl x t1 t2) = show t2

freeVars :: Term -> [Sym]
freeVars (Srt _) = []
freeVars (Var x) = [x]
freeVars (Lam x ty tm) = ((freeVars tm) \\ [x]) `union` (freeVars ty)
freeVars (App lhs rhs) = (freeVars lhs) `union` (freeVars rhs)
freeVars (Pi x lTy rTy) = ((freeVars rTy) \\ [x]) `union` (freeVars lTy)

data Sort
  = Star
  | Box
  deriving (Eq)

instance Show Sort where
  show Star = "*"
  show Box = "[]"