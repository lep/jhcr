{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}



module Jass.Ast
    ( Ast (..)
    , Expr
    , Stmt
    , LVar
    , VarDef
    , Toplevel
    , Programm
    , Name
    , Type
    , Lit
    , Constant (..)
    , fmap, foldMap, traverse
    , s2i, s2r, rawcode2int
    , eliminateElseIfs
    ) where

import Prelude hiding (fmap, foldMap, traverse)

import Data.Composeable
import Data.Monoid

import Data.Foldable hiding (foldMap)
import qualified Data.Foldable as F

import Data.Functor hiding (fmap)
import qualified Data.Functor as F

import Data.Traversable hiding (traverse)
import qualified Data.Traversable as T

import Control.Applicative
import Control.Arrow

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BL

import Unsafe.Coerce

import Data.Int
import Data.Char

import Data.Hashable

import GHC.Generics
import Data.Binary


data Expr
data Stmt
data LVar
data VarDef
data Toplevel
data Block
data Programm

data Constant = Const | Normal
    deriving (Eq, Ord, Show, Generic)

instance Binary Constant
instance Hashable Constant

type Name = String
type Type = String
type Lit = String

data Ast var a where
    Programm :: [Ast var Toplevel] -> Ast var Programm
    Native :: Constant -> var -> [(Type, var)] -> Type -> Ast var Toplevel
    Function :: Constant -> var -> [(Type, var)] -> Type -> [Ast var Stmt] -> Ast var Toplevel
    Global :: Ast var VarDef -> Ast var Toplevel
    Typedef :: Type -> Type -> Ast var Toplevel


    Set :: Ast var LVar -> Ast var Expr -> Ast var Stmt
    Local :: Ast var VarDef -> Ast var Stmt
    If :: Ast var Expr -> [Ast var Stmt] -> [(Ast var Expr, [Ast var Stmt])] -> Maybe [Ast var Stmt] -> Ast var Stmt
    Loop :: [Ast var Stmt] -> Ast var Stmt
    Exitwhen :: Ast var Expr -> Ast var Stmt
    Return :: Maybe (Ast var Expr) -> Ast var Stmt

    Call :: var -> [Ast var Expr] -> Ast var a

    Var :: Ast var LVar -> Ast var Expr
    Int :: Lit -> Ast var Expr
    Rawcode :: Lit -> Ast var Expr
    Real :: Lit -> Ast var Expr
    Bool :: Bool -> Ast var Expr
    String :: Lit -> Ast var Expr
    Code :: var -> Ast var Expr
    Null :: Ast var Expr

    AVar :: var -> Ast var Expr -> Ast var LVar
    SVar :: var -> Ast var LVar

    ADef :: var -> Type -> Ast var VarDef
    SDef :: Constant -> var -> Type -> Maybe (Ast var Expr) -> Ast var VarDef

deriving instance Show var => Show (Ast var a)
deriving instance Eq var => Eq (Ast var a)

newtype HashMonoid = HashMonoid { getHash :: Int }

instance Semigroup HashMonoid where
    HashMonoid a <> HashMonoid b = HashMonoid $ hashWithSalt a b

instance Monoid HashMonoid where
    mempty = HashMonoid 0

h :: Hashable h => h -> HashMonoid
h x = HashMonoid $ hash x

instance Hashable var => Hashable (Ast var a) where
    hashWithSalt salt x = hashWithSalt salt $ hash x
    hash x = getHash $
      case x of
        Programm p -> h (-3 :: Int) <> F.foldMap h p
        Native c v args ret ->
            h (-2 :: Int) <> h c <> h v
            <> F.foldMap h args <> h ret
        Function c v args ret body ->
            h (-1 :: Int) <> h c <> h v
            <> F.foldMap h args <> h ret <> F.foldMap h body
        Global vdef -> h (0::Int) <> h vdef
        Typedef a b -> h (1::Int) <> h a <> h b
        Set lvar expr -> h (2::Int) <> h lvar <> h expr
        Local vdef -> h (3::Int) <>h vdef
        If cond tb elseifs eb -> h (4::Int) <> h cond <> F.foldMap h tb <> F.foldMap h elseifs <> h eb
        Loop body -> h (5::Int) <> h body
        Exitwhen expr -> h (6::Int) <> h expr
        Return expr -> h (7::Int) <> h expr
        Call v args -> h (8::Int) <> h v <> h args
        Var lvar -> h (9::Int) <> h lvar
        Int i -> h (10::Int) <> h i
        Real r -> h (11::Int) <> h r
        Bool b -> h (12::Int) <> h b
        Rawcode r -> h (13::Int) <> h r
        String s -> h (14::Int) <> h s
        Code c -> h (15::Int) <> h c
        Null -> h (16::Int)
        
        AVar v idx -> h (17::Int) <> h v <> h idx
        SVar v -> h (18::Int) <> h v
        
        ADef v ty -> h (19::Int) <> h v <> h ty
        SDef c v ty init -> h (20::Int) <> h c <> h v <> h ty <> h init

instance Compose (Ast var) where
    compose f a =
      case a of
        Programm toplvl -> Programm <$> T.traverse f toplvl
        Function c n a r body -> Function c n a r <$> T.traverse f body
        Global var -> Global <$> f var
        Set x y -> Set <$> f x <*> f y
        Local x -> Local <$> f x
        If e tb elseifs eb -> If <$> f e <*> T.traverse f tb <*> T.traverse composeEIf elseifs <*> T.traverse (T.traverse f) eb
          where
            composeEIf (cond, block) = (,) <$> f cond <*> T.traverse f block
        Loop b -> Loop <$> T.traverse f b
        Exitwhen cond -> Exitwhen <$> f cond
        Return (Just e) -> Return . Just <$> f e
        Call n args -> Call <$> pure n <*> T.traverse f args
        Var lvar -> Var <$> f lvar
        AVar n ix -> AVar <$> pure n <*> f ix
        SDef c n t (Just e) -> SDef c n t . Just <$> f e
        x -> pure x

newtype FAst a var = FAst { getAst :: Ast var a }

instance Functor (FAst a) where
    fmap f x = FAst $
      case getAst x of
        Programm prog -> Programm (map (getAst . F.fmap f . FAst) prog)
        Native c name args ret -> Native c (f name) (map (second f) args) ret
        Function c name args ret body ->
          Function c
                   (f name)
                   (map (second f) args)
                   ret
                   (map (getAst . F.fmap f . FAst) body)
        Global vdef -> Global ((getAst . F.fmap f . FAst) vdef)
        Set lvar expr -> Set ((getAst . F.fmap f . FAst) lvar) ((getAst . F.fmap f . FAst) expr)
        Local vdef -> Local ((getAst . F.fmap f . FAst) vdef)
        If e ib ei eb ->
          If ((getAst . F.fmap f . FAst) e)
             (map (getAst . F.fmap f . FAst) ib)
             (map ((getAst . F.fmap f . FAst) *** (map (getAst . F.fmap f . FAst))) ei)
             (F.fmap (map (getAst . F.fmap f . FAst)) eb)
        Loop body -> Loop (map (getAst . F.fmap f . FAst) body)
        Exitwhen expr -> Exitwhen ((getAst . F.fmap f . FAst) expr)
        Return expr -> Return (F.fmap (getAst . F.fmap f . FAst) expr)
        Call name args -> Call (f name) (map (getAst . F.fmap f . FAst) args)
        Var lvar -> Var ((getAst . F.fmap f . FAst) lvar)

        AVar v idx  -> AVar (f v) ((getAst . F.fmap f . FAst) idx)
        SVar v      -> SVar (f v)

        Code v      -> Code (f v)

        ADef v typ      -> ADef (f v) typ
        SDef c v typ expr -> SDef c (f v) typ (F.fmap (getAst . F.fmap f . FAst) expr)

        _ -> unsafeCoerce x

    {-
      where
        f' :: Ast a r -> Ast b r
        f' = getAst . F.fmap f . FAst
    -}

instance Foldable (FAst r) where
    foldMap :: Monoid m => (a -> m) -> FAst r a -> m
    foldMap f x =
      case getAst x of
        Programm prog -> F.foldMap (F.foldMap f . FAst) prog
        Native c name args ret ->
            f name <> F.foldMap (f . snd) args
        Function c name args ret body ->
            f name <> F.foldMap (f . snd) args <> F.foldMap (F.foldMap f . FAst) body
        Global vdef -> (F.foldMap f . FAst) vdef
        Set lvar expr -> (F.foldMap f . FAst) lvar <> (F.foldMap f . FAst) expr
        Local vdef -> (F.foldMap f . FAst) vdef
        If e ib ei eb ->
            (F.foldMap f . FAst) e
                <> F.foldMap (F.foldMap f . FAst) ib
                <> F.foldMap (uncurry (<>) . ((F.foldMap f . FAst) *** F.foldMap (F.foldMap f . FAst))) ei
                 <> F.foldMap (F.foldMap (F.foldMap f . FAst)) eb
        Loop body -> F.foldMap (F.foldMap f . FAst) body
        Exitwhen expr -> (F.foldMap f . FAst) expr
        Return expr -> F.foldMap (F.foldMap f . FAst) expr
        Call name args -> f name <> F.foldMap (F.foldMap f . FAst) args
        Var lvar -> (F.foldMap f . FAst) lvar

        Code v      -> f v
        AVar v idx  -> f v <> (F.foldMap f . FAst) idx
        SVar v      -> f v
        ADef v typ  -> f v
        SDef c v typ expr -> f v <> F.foldMap (F.foldMap f . FAst) expr

        _ -> mempty
        {-
          where
            f' :: Monoid m => Ast a r -> m
            f' = (F.foldMap f . FAst)
        -}

instance Traversable (FAst r) where
    traverse :: Applicative f => (a -> f b) -> FAst r a -> f (FAst r b)
    traverse f x = liftA FAst $
      case getAst x of
        Programm prog -> Programm <$> T.traverse (liftA getAst . T.traverse f . FAst) prog
        Native c name args ret ->
          Native c <$> f name <*> T.traverse (fTypeAndName f) args <*> pure ret
        Function c name args ret body ->
          Function c <$> f name <*> T.traverse (fTypeAndName f) args <*> pure ret <*> T.traverse (liftA getAst . T.traverse f . FAst) body
        Global vdef -> Global <$> (liftA getAst . T.traverse f . FAst) vdef
        Set lvar expr -> Set <$> (liftA getAst . T.traverse f . FAst) lvar <*> (liftA getAst . T.traverse f . FAst) expr
        Local vdef -> Local <$> (liftA getAst . T.traverse f . FAst) vdef
        If e ib ei eb ->
          If <$> (liftA getAst . T.traverse f . FAst) e
            <*> T.traverse (liftA getAst . T.traverse f . FAst) ib
            <*> T.traverse (\(c, b) -> (,) <$> (liftA getAst . T.traverse f . FAst) c <*> T.traverse (liftA getAst . T.traverse f . FAst) b) ei
            <*> T.traverse (T.traverse (liftA getAst . T.traverse f . FAst)) eb
        Loop body -> Loop <$> T.traverse (liftA getAst . T.traverse f . FAst) body
        Exitwhen cond -> Exitwhen <$> (liftA getAst . T.traverse f . FAst) cond
        Return expr -> Return <$>  T.traverse (liftA getAst . T.traverse f . FAst) expr
        Var lvar -> Var <$> (liftA getAst . T.traverse f . FAst) lvar

        Call name args    -> Call <$> f name <*> T.traverse (liftA getAst . T.traverse f . FAst) args
        Code name         -> Code <$> f name
        AVar v idx        -> AVar <$> f v <*> (liftA getAst . T.traverse f . FAst) idx
        SVar v            -> SVar <$> f v
        ADef v typ        -> ADef <$> f v <*> pure typ
        SDef c v typ expr -> SDef c <$> f v <*> pure typ <*> T.traverse (liftA getAst . T.traverse f . FAst) expr

        n -> pure $ unsafeCoerce n
      where
        fTypeAndName f (ty, n) = sequenceA (ty, f n)
    {-
      where
        f' = (liftA getAst . T.traverse f . FAst)
    -}

fmap :: (a -> b) -> Ast a r -> Ast b r
fmap f = getAst . F.fmap f . FAst

foldMap :: Monoid m => (a -> m) -> Ast a r -> m
foldMap f = F.foldMap f . FAst

traverse :: Applicative f => (a -> f b) -> Ast a r -> f (Ast b r)
traverse f = liftA getAst . T.traverse f . FAst


s2i :: Lit -> Int32
s2i = go 
  where
    go ('-':s) = negate $ go s
    go ('+':s) = go s
    go ('$':s) = read $ "0x" <> s
    go s = read s

s2r :: Lit -> Float
s2r = go 
  where
    go ('-':s) = negate $ go s
    go ('+':s) = go s
    go s = read $ "0" <> s <> "0"

rawcode2int :: Lit -> Int32
rawcode2int = foldl (\acc word -> acc*256 + fromIntegral (ord word)) 0 . ex
  where
    ex = drop 1 . reverse . drop 1 . reverse

eliminateElseIfs :: Ast v Stmt -> Ast v Stmt
eliminateElseIfs (If cond tb eis eb) =
    If cond tb [] $ foldr (\(cond, body) elem -> Just [If cond body [] elem]) eb eis
eliminateElseIfs x = x
