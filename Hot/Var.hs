{-# LANGUAGE OverloadedStrings #-}

module Hot.Var
    ( Var(..)
    , mkLocal, mkGlobal, mkOp, mkFn
    , nameOf, getId, getId', isConst
    , (##)
    ) where

import Data.Int
import Data.Monoid
import Data.String

import Data.ByteString.Lazy (ByteString)

import Jass.Ast (Name, Type, Constant(..))


data Var = Local Name Type Bool Int32
         | Global Constant Name Type Bool Int32
         | Op Name
         | Fn Name [Type] Type Int32
    deriving (Eq, Ord, Show)

mkLocal n = Local n "_void" False 0

mkGlobal n = Global Normal n "_void" False 0

mkFn n = Fn n [] "_void" 0

mkOp n = Op n

(##) :: Name -> Var -> Var
n ## v =
  case v of
    Local name ty isarray id -> Local (n <> name) ty isarray id
    Global c name ty isarray id -> Global c (n <> name) ty isarray id
    Op name -> v
    Fn name args ret id -> Fn (n <> name) args ret id

nameOf :: Var -> Name
nameOf v =
  case v of
    Local name _ _ _ -> name
    Global _ name _ _ _ -> name
    Op name -> name
    Fn name _ _ _ -> name

getId :: Integral a => Var -> a
getId v =
  case v of
    Local _ _ _ id -> fromIntegral id
    Global _ _ _ _ id -> fromIntegral id
    Fn _ _ _ id -> fromIntegral id
    

getId' :: Var -> ByteString
getId' = fromString . show . getId

isConst :: Var -> Bool
isConst (Global Const _ _ _ _) = True
isConst _ = False
