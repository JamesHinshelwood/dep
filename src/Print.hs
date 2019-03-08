module Print where

import Prelude hiding ((<>))
import Syntax
import Text.PrettyPrint (Doc, (<+>), (<>), text, colon, char, parens, ($$), comma, brackets, hsep)
import Unbound.LocallyNameless hiding (Refl)
import Data.Set hiding (map)
import Data.List

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

class Pretty a where
  pp :: LFresh m => a -> m Doc

instance Pretty (Name a) where
  pp = return . text . show

instance Pretty Term where
  pp Type = return $ text "Type"
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
  pp (Variant l t ty) = do
    t' <- pp t
    ty' <- pp ty
    return $ (text l) <> (parens t') <+> colon <+> ty'
  pp (Sum l) = do
    l' <- mapM (\(x, t) -> ((text x) <>) <$> parens <$> (pp t)) l
    return $ hsep $ intersperse plus l'
  pp (Case s l) = do
    s' <- pp s
    l' <- mapM (\(lbl, b) -> lunbind b $ \(x, t) -> do
      x' <- pp x
      t' <- pp t
      return $ (text lbl) <> (parens x') <+> arrow <+> t') l
    return $ (text "case") <+> s' <+> (text "of") <+> (hsep $ intersperse mid l')
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