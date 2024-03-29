{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Hot.Var
    ( Var(..)
    , mkLocal, mkGlobal, mkOp, mkFn
    , nameOf, getId, getId'
    , getReplacement
    , (##)
    ) where

import Data.Int

import GHC.Generics
import Data.Binary


import Jass.Ast (Name, Type, Lit, Constant(..))


data Var = Local Name Type Bool Int32
         | Global Constant Name Type Bool Int32
         | Op Name
         | Fn Name [Type] Type Int32 (Maybe Int32)
    deriving (Eq, Ord, Show, Generic)

instance Binary Var

mkLocal :: Name -> Var
mkLocal n = Local n "_void" False 0

mkGlobal :: Name -> Var
mkGlobal n = Global Normal n "_void" False 0

mkFn :: Name -> Var
mkFn n = Fn n [] "_void" 0 Nothing

mkOp :: Name -> Var
mkOp n = Op n

(##) :: Name -> Var -> Var
n ## v =
  case v of
    Local name ty isarray id -> Local (n <> name) ty isarray id
    Global c name ty isarray id -> Global c (n <> name) ty isarray id
    Op _ -> v
    Fn name args ret id r -> Fn (n <> name) args ret id r

nameOf :: Var -> Name
nameOf v =
  case v of
    Local name _ _ _ -> name
    Global _ name _ _ _ -> name
    Op name -> name
    Fn name _ _ _ _ -> name

getId :: Integral a => Var -> a
getId v =
  case v of
    Local _ _ _ id -> fromIntegral id
    Global _ _ _ _ id -> fromIntegral id
    Fn _ _ _ id _ -> fromIntegral id

getReplacement :: Integral i => Var -> Maybe i
getReplacement v =
  case v of
    Fn _ _ _ _ r -> fmap fromIntegral r
    _ -> Nothing
    

getId' :: Var -> Lit
getId' =  show . getId
