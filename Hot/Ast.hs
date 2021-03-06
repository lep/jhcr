{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hot.Ast
    ( Programm, Toplevel, LVar, Stmt, Expr, Name, Type
    , Var (..)
    , Ast (..)
    , jass2hot
    , globals2statements
    ) where

import Data.Int
import Data.Maybe 

import qualified Jass.Ast as Jass
import Jass.Ast (Programm, Toplevel, LVar, Stmt, Expr, Name, Type)

import Hot.Var

import Data.Hashable

data Ast var a where
    Programm :: [Ast var Toplevel] -> Ast var Programm
    Function :: var -> [(Type, var)] -> Type -> Ast var Stmt -> Ast var Toplevel


    Block :: [Ast var Stmt] -> Ast var Stmt

    Set :: Ast var LVar -> Ast var Expr -> Ast var Stmt
    If :: Ast var Expr -> Ast var Stmt -> Ast var Stmt -> Ast var Stmt
    Loop :: Ast var Stmt -> Ast var Stmt
    Exitwhen :: Ast var Expr -> Ast var Stmt
    Return :: Maybe (Ast var Expr) -> Ast var Stmt

    Call :: var -> [Ast var Expr] -> Ast var a

    Var :: Ast var LVar -> Ast var Expr
    Int :: Int32 -> Ast var Expr
    Real :: Float -> Ast var Expr
    Bool :: Bool -> Ast var Expr
    String :: Jass.Lit -> Ast var Expr
    Code :: var -> Ast var Expr
    Null :: Ast var Expr

    AVar :: var -> Ast var Expr -> Ast var LVar
    SVar :: var -> Ast var LVar

deriving instance (Show var) => Show (Ast var a)
deriving instance (Eq var) => Eq (Ast var a)

instance Hashable var => Hashable (Ast var a) where
    hashWithSalt salt x = hashWithSalt salt $ hash x
    hash x =
      case x of
        Programm p -> hashWithSalt 1 p
        Function n args ret body ->
            hashWithSalt 2 [hash n, hash args, hash ret, hash body]
        Block b -> hashWithSalt 3 b
        Set lvar expr -> hashWithSalt 4 [hash lvar, hash expr]
        If cond tb eb -> hashWithSalt 5 [hash cond, hash tb, hash eb]
        Loop body -> hashWithSalt 6 body
        Exitwhen expr -> hashWithSalt 7 expr
        Return expr -> hashWithSalt 8 expr
        Call n args -> hashWithSalt 9 [hash n, hash args]
        Var lvar -> hashWithSalt 10 lvar
        Int i -> hashWithSalt 11 i
        Real r -> hashWithSalt 12 r
        Bool b -> hashWithSalt 13 b
        String s -> hashWithSalt 14 s
        Code c -> hashWithSalt 15 c
        Null -> hashWithSalt 16 ()
        AVar v idx -> hashWithSalt 17 [hash v, hash idx]
        SVar v -> hashWithSalt 18 v

convert :: Jass.Ast Var a -> Ast Var a
convert e =
  case e of
    Jass.Call fn args -> Call fn $ fmap convert args
    Jass.Var v -> Var $ convert v
    Jass.Int i -> Int $ Jass.s2i i
    Jass.Rawcode i -> Int $ Jass.rawcode2int i
    Jass.Real r -> Real $ Jass.s2r r
    Jass.Bool b -> Bool b
    Jass.String s -> String s
    Jass.Code c -> Code c
    Jass.Null -> Null
    Jass.AVar n idx -> AVar n $ convert idx
    Jass.SVar v -> SVar v

convertStmt :: Jass.Ast Var a -> Maybe (Ast Var a)
convertStmt e =
  case e of
    Jass.Set lvar expr -> Just $ Set (convert lvar) (convert expr)
    Jass.If{} -> Just $ convertIfElse $ Jass.eliminateElseIfs e
    Jass.Loop body -> Just $ Loop $ Block $ mapMaybe convertStmt body
    Jass.Exitwhen cond -> Just $ Exitwhen $ convert cond
    Jass.Return e -> Just $ Return $ fmap convert e
    Jass.Call fn args -> Just $ Call fn $ fmap convert args
    Jass.Local (Jass.SDef _ n _ (Just e)) -> convertStmt $ Jass.Set (Jass.SVar n) e

    _ -> Nothing




convertIfElse :: Jass.Ast Var Stmt -> Ast Var Stmt
convertIfElse (Jass.If cond body [] elseB) =
    If (convert cond)
       (Block $ mapMaybe convertStmt body)
       (Block $ mapMaybe convertStmt $ concat elseB)



globals2statements :: Jass.Ast Var Programm -> [Ast Var Stmt]
globals2statements (Jass.Programm p) = mapMaybe go p
  where
    go :: Jass.Ast Var a -> Maybe (Ast Var Stmt)
    go e =
      case e of
        Jass.Global x@(Jass.SDef _ _ _ (Just _)) -> convertStmt $ Jass.Local x
        _ -> Nothing

jass2hot:: Jass.Ast Var Programm -> Ast Var Programm
jass2hot (Jass.Programm p) = Programm $ mapMaybe go p
  where
    go :: Jass.Ast Var a -> Maybe (Ast Var a)
    go e =
      case e of
        Jass.Native{} -> Nothing
        Jass.Typedef{} -> Nothing
        Jass.Global{} -> Nothing

        Jass.Function _ n args ret body -> Just $ Function n args ret $ Block $ mapMaybe convertStmt body

