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
    , isGlobal, isLocal, isFunction, isOp
    ) where

import Prelude hiding (fmap, foldMap, traverse)

import Data.Composeable

import qualified Data.Foldable as F
import qualified Data.Functor as F
import qualified Data.Traversable as T

import Control.Applicative
import Control.Arrow

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


instance Hashable var => Hashable (Ast var a) where
    hashWithSalt salt x = hashWithSalt salt $ hash x
    hash x =
      case x of
        Programm p -> hashWithSalt 1 p
        Native c v args ret ->
            hashWithSalt 2 [hash c, hash v, hash args, hash ret]
        Function c v args ret body ->
            hashWithSalt 3 [hash c, hash v, hash args, hash ret, hash body]
        Global vdef -> hashWithSalt 4 vdef
        Typedef a b -> hashWithSalt 5 [hash a, hash b]
        Set lvar expr -> hashWithSalt 6 [hash lvar, hash expr]
        Local vdef -> hashWithSalt 7 vdef
        If cond tb elseifs eb ->
            hashWithSalt 8 [hash cond, hash tb, hash elseifs, hash eb]
        Loop body -> hashWithSalt 9 body
        Exitwhen expr -> hashWithSalt 10 expr
        Return expr -> hashWithSalt 11 expr
        Call v args -> hashWithSalt 12 [hash v, hash args]
        Var lvar -> hashWithSalt 13 lvar
        Int i -> hashWithSalt 14 i
        Real r -> hashWithSalt 15 r
        Bool b -> hashWithSalt 16 b
        Rawcode r -> hashWithSalt 17 r
        String s -> hashWithSalt 18 s
        Code c -> hashWithSalt 19 c
        Null -> hashWithSalt 20 ()
        AVar v idx -> hashWithSalt 21 [hash v, hash idx]
        SVar v -> hashWithSalt 22 v
        ADef v ty -> hashWithSalt 23 [hash v, hash ty]
        SDef c v ty init -> hashWithSalt 24 [hash c, hash v, hash ty, hash init]


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
    traverse f x = F.fmap FAst $
      case getAst x of
        Programm prog -> Programm <$> T.traverse (F.fmap getAst . T.traverse f . FAst) prog
        Native c name args ret ->
          Native c <$> f name <*> T.traverse (fTypeAndName f) args <*> pure ret
        Function c name args ret body ->
          Function c <$> f name <*> T.traverse (fTypeAndName f) args <*> pure ret <*> T.traverse (F.fmap getAst . T.traverse f . FAst) body
        Global vdef -> Global <$> (F.fmap getAst . T.traverse f . FAst) vdef
        Set lvar expr -> Set <$> (F.fmap getAst . T.traverse f . FAst) lvar <*> (F.fmap getAst . T.traverse f . FAst) expr
        Local vdef -> Local <$> (F.fmap getAst . T.traverse f . FAst) vdef
        If e ib ei eb ->
          If <$> (F.fmap getAst . T.traverse f . FAst) e
            <*> T.traverse (F.fmap getAst . T.traverse f . FAst) ib
            <*> T.traverse (\(c, b) -> (,) <$> (F.fmap getAst . T.traverse f . FAst) c <*> T.traverse (F.fmap getAst . T.traverse f . FAst) b) ei
            <*> T.traverse (T.traverse (F.fmap getAst . T.traverse f . FAst)) eb
        Loop body -> Loop <$> T.traverse (F.fmap getAst . T.traverse f . FAst) body
        Exitwhen cond -> Exitwhen <$> (F.fmap getAst . T.traverse f . FAst) cond
        Return expr -> Return <$>  T.traverse (F.fmap getAst . T.traverse f . FAst) expr
        Var lvar -> Var <$> (F.fmap getAst . T.traverse f . FAst) lvar

        Call name args    -> Call <$> f name <*> T.traverse (F.fmap getAst . T.traverse f . FAst) args
        Code name         -> Code <$> f name
        AVar v idx        -> AVar <$> f v <*> (F.fmap getAst . T.traverse f . FAst) idx
        SVar v            -> SVar <$> f v
        ADef v typ        -> ADef <$> f v <*> pure typ
        SDef c v typ expr -> SDef c <$> f v <*> pure typ <*> T.traverse (F.fmap getAst . T.traverse f . FAst) expr

        n -> pure $ unsafeCoerce n
      where
        fTypeAndName f (ty, n) = sequenceA (ty, f n)
    {-
      where
        f' = (F.fmap getAst . T.traverse f . FAst)
    -}

fmap :: (a -> b) -> Ast a r -> Ast b r
fmap f = getAst . F.fmap f . FAst

foldMap :: Monoid m => (a -> m) -> Ast a r -> m
foldMap f = F.foldMap f . FAst

traverse :: Applicative f => (a -> f b) -> Ast a r -> f (Ast b r)
traverse f = F.fmap getAst . T.traverse f . FAst


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
rawcode2int = foldl (\acc word -> acc*256 + fromIntegral (ord word)) 0


eliminateElseIfs :: Ast v Stmt -> Ast v Stmt
eliminateElseIfs (If cond tb eis eb) =
    If cond tb [] $ foldr (\(cond, body) elem -> Just [If cond body [] elem]) eb eis
eliminateElseIfs x = x

isGlobal :: Ast a Toplevel -> Bool
isGlobal Global{} = True
isGlobal _ = False

isLocal :: Ast a Stmt -> Bool
isLocal Local{} = True
isLocal _ = False


isFunction :: Ast a Toplevel -> Bool
isFunction Function{} = True
isFunction _ = False


isOp x = x `elem` ["and", "or", "not"
                  , "+", "-", "*", "/", "%"
                  , "==", "!=", "<=", ">=", "<", ">"
                  ]