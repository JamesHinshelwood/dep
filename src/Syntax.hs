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
  | InL Term Term
  | InR Term Term
  | Sum Term Term
  | Case Term (Bind (Name Term) Term) (Bind (Name Term) Term)
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

case_ :: Term -> String -> Term -> String -> Term -> Term
case_ s x t y u = Case s (bind (string2Name x) t) (bind (string2Name y) u)

split :: Term -> Term -> String -> Term -> Term
split c p z t = Split c p (bind (string2Name z) t)