{-# LANGUAGE MultiParamTypeClasses,
             TemplateHaskell,
             ScopedTypeVariables,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances
#-}

module Syntax where

import Prelude hiding ((<>))

import Unbound.LocallyNameless hiding (Refl)
import Data.Set

import Text.PrettyPrint (Doc, (<+>), (<>), text, colon, char, parens, ($$), comma, brackets)

slash :: Doc
slash = char '\\'

dot :: Doc
dot = char '.'

arrow :: Doc
arrow = text "->"

equals :: Doc
equals = char '='

mid :: Doc
mid = char '|'

star :: Doc
star = char '*'

plus :: Doc
plus = char '+'

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


class Pretty a where
  pp :: LFresh m => a -> m Doc

instance Pretty (Name a) where
  pp = return . text . show

instance Pretty Term where
  pp (Type) = return $ text "Type"
  pp (Var x) = pp x
  pp (Lam b) = lunbind b $ \((x, Embed ty), tm) -> do
    x' <- pp x
    ty' <- pp ty
    tm' <- pp tm
    return $ parens $ slash <> x' <+> colon <+> ty' <> dot <+> tm'
  pp (App t1 t2) = do
    t1' <- pp t1
    t2' <- pp t2
    return $ parens $ t1' <+> t2'
  pp (Pi b) = lunbind b $ \((x, Embed t1), t2) -> do
    x' <- pp x
    t1' <- pp t1
    t2' <- pp t2
    if x `notMember` fv t2
      then return $ t1' <+> arrow <+> t2'
      else return $ (parens $ x' <+> colon <+> t1') <+> arrow <+> t2'
  pp (Let b) = lunbind b $ \((x, Embed t1), t2) -> do
    x' <- pp x
    t1' <- pp t1
    t2' <- pp t2
    return $ (text "let") <+> x' <+> equals <+> t1' <+> (text "in") $$ t2'
  pp (Decl b) = lunbind b $ \((x, Embed ty), tm) -> do
    x' <- pp x
    ty' <- pp ty
    tm' <- pp tm
    return $ (text "let") <+> x' <+> colon <+> ty' <+> (text "in") $$ tm'
  pp (TypedPair t1 t2 ty) = do
    t1' <- pp t1
    t2' <- pp t2
    ty' <- pp ty
    return $ parens $ t1' <> comma <+> t2' <+> colon <+> ty'
  pp (Sigma b) = lunbind b $ \((x, Embed t1), t2) -> do
    x' <- pp x
    t1' <- pp t1
    t2' <- pp t2
    return $ parens $ (parens $ x' <+> colon <+> t1') <+> star <+> t2'
  pp (First t) = do
    t' <- pp t
    return $ t' <> dot <> (char '1')
  pp (Second t) = do
    t' <- pp t
    return $ t' <> dot <> (char '2')
  pp (InL tm ty) = do
    tm' <- pp tm
    ty' <- pp ty
    return $ (text "inl") <+> tm' <+> colon <+> ty'
  pp (InR tm ty) = do
    tm' <- pp tm
    ty' <- pp ty
    return $ (text "inr") <+> tm' <+> colon <+> ty'
  pp (Sum t1 t2) = do
    t1' <- pp t1
    t2' <- pp t2
    return $ parens $ t1' <+> plus <+> t2'
  pp (Case s b1 b2) = lunbind b1 $ \(x, t) -> lunbind b2 $ \(y, u) -> do
    s' <- pp s
    x' <- pp x
    t' <- pp t
    y' <- pp y
    u' <- pp u
    return $ (text "case") <+> s' <+> (text "of") <+>
      (text "inl") <+> x' <+> arrow <+> t' <+> mid <+>
      (text "inr") <+> y' <+> arrow <+> u'
  pp (Unit) = return $ text "unit"
  pp (UnitTy) = return $ text "Unit"
  pp (Eq x y ty) = do
    x' <- pp x
    y' <- pp y
    ty' <- pp ty
    return $ parens $ x' <+> equals <+> y' <+> colon <+> ty'
  pp (Refl ty) = do
    ty' <- pp ty
    return $ parens $ (text "refl") <+> colon <+> ty'
  pp (Split c p b) = lunbind b $ \(z, t) -> do
    c' <- pp c
    p' <- pp p
    z' <- pp z
    t' <- pp t
    return $ (text "case") <> (brackets c') <+> p' <+> (text "of") <+> (text "refl") <> (parens z') <+> arrow <+> t'