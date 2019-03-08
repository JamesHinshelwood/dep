{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             ScopedTypeVariables,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances
#-}

module Syntax where

import Unbound.LocallyNameless hiding (Refl)

data Term
  = Type
  | Var (Name Term)
  | Lam (Bind (Name Term, Embed Term) Term)
  | App Term Term
  | Pi (Bind (Name Term, Embed Term) Term)
  | Let (Bind (Name Term, Embed Term) Term)
  | Decl (Bind (Name Term, Embed Term) Term)
  | TypedPair Term Term Term
  | Sigma (Bind (Name Term, Embed Term) Term)
  | First Term
  | Second Term
  | Variant String Term Term
  | Sum [(String, Term)]
  | Case Term [(String, Bind (Name Term) Term)]
  | Unit
  | UnitTy
  | Eq Term Term Term
  | Refl Term
  | Split Term Term (Bind (Name Term) Term)
  deriving (Show)

$(derive [''Term])

instance Alpha Term where

instance Subst Term Term where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing

var :: String -> Term
var = Var . string2Name

lam :: String -> Term -> Term -> Term
lam = binding Lam

pi_ :: String -> Term -> Term -> Term
pi_ = binding Pi

let_ :: String -> Term -> Term -> Term
let_ = binding Let

decl :: String -> Term -> Term -> Term
decl = binding Decl

sigma :: String -> Term -> Term -> Term
sigma = binding Sigma

binding :: (Bind (Name Term, Embed Term) Term -> Term) -> String -> Term -> Term -> Term
binding con x t1 t2 = con $ bind (string2Name x, embed t1) t2

case_ :: Term -> [(String, String, Term)] -> Term
case_ s l = Case s $ map (\(lbl, x, t) -> (lbl, bind (string2Name x) t)) l

split :: Term -> Term -> String -> Term -> Term
split c p z t = Split c p (bind (string2Name z) t)