
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hot.Instruction
    ( Instruction (..)
    , Register, Label
    , serialize
    ) where

import qualified Hot.Ast as Hot
import qualified Hot.Types as Hot
import Hot.Ast (Programm, Toplevel, LVar, Stmt, Expr, Name, Type)
import Hot.Ast ( Var(..) )



import Data.Composeable

import Control.Arrow (second)

import Data.List (intersperse)

import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Data.DList (DList)
import qualified Data.DList as DList

import Data.Int

import Data.Maybe 

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder

import Control.Lens
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import Unsafe.Coerce (unsafeCoerce)

import Debug.Trace

--type Register = Int32
--type Label = Int32

newtype Register = Register Int16
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

reg :: Register -> Builder
reg (Register r) = int16Dec r


newtype Label = Lbl Int32
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

label :: Label -> Builder
label (Lbl l) = int32Dec l

data Instruction
    -- three regs
    = Lt Type Register Register Register
    | Le Type Register Register Register
    | Gt Type Register Register Register
    | Ge Type Register Register Register
    | Eq Type Register Register Register
    | Neq Type Register Register Register
    | Add Type Register Register Register
    | Sub Type Register Register Register
    | Mul Type Register Register Register
    | Div Type Register Register Register
    
    | SetGlobalArray Type Name Register Register
    | GetGlobalArray Type Register Name Register
    | SetArray Type Register Register Register
    | GetArray Type Register Register Register
    
    -- two regs
    | Negate Type Register Register
    | Set Type Register Register
    | SetGlobal Type Name Register
    | GetGlobal Type Register Name
    | Bind Type Register Register

    -- special
    | Literal Type Register (Hot.Ast Var Expr)
    | Call Type Register Name
    | Convert Type Register Type Register

    -- one label
    | Label Label
    | Jmp Label
    | Function Name
    
    | JmpT Label Register -- this is encoded as jmpt reg label
    
    | Not Register Register
    
    | Ret
    deriving (Show)


serialize :: [Instruction] -> Builder
serialize = unlines . map s
  where
    unlines = mconcat . intersperse (charUtf8 '\n')
    unwords = mconcat . intersperse (charUtf8 ' ')

    serializeLit :: Hot.Ast Var Expr -> Builder
    serializeLit l =
      case l of
        Hot.Int i -> int32Dec i
        Hot.Real r -> floatDec r
        Hot.String s -> lazyByteString s
        Hot.Bool s -> stringUtf8 $ show s
        Hot.Null -> stringUtf8 "null"

    bla ins args = unwords [ins, unwords args]

    --typeToId x = int32Dec (Hot.types Map.! x)
    typeToId x = lazyByteString   x

    s ins =
      case ins of
        Lt t s a b -> unwords [ "lt", typeToId t, reg s, reg a, reg b]
        Le t s a b -> unwords [ "le", typeToId t, reg s, reg a, reg b]
        Gt t s a b -> unwords [ "gt", typeToId t, reg s, reg a, reg b]
        Ge t s a b -> unwords [ "ge", typeToId t, reg s, reg a, reg b]
        Eq t s a b -> unwords [ "eq", typeToId t, reg s, reg a, reg b]
        Neq t s a b -> unwords [ "neq", typeToId t, reg s, reg a, reg b]
        Add t s a b -> unwords [ "add", typeToId t, reg s, reg a, reg b]
        Sub t s a b -> unwords [ "sub", typeToId t, reg s, reg a, reg b]
        Mul t s a b -> unwords [ "mul", typeToId t, reg s, reg a, reg b]
        Div t s a b -> unwords [ "div", typeToId t, reg s, reg a, reg b]

        Negate t s a -> bla "neg" [typeToId t, reg s, reg a]

        Set t s a -> unwords [ "set", typeToId t, reg s, reg a]
        SetGlobal ty g s -> unwords [ "sg", typeToId ty, lazyByteString g, reg s]
        GetGlobal ty s g -> unwords [ "gg", typeToId ty, reg s, lazyByteString g]

        SetArray ty ar idx r -> unwords [ "sla", typeToId ty, reg ar, reg idx, reg r]
        GetArray ty t ar idx -> unwords [ "gla", typeToId ty, reg t, reg ar, reg idx]

        SetGlobalArray t ar idx v -> unwords [ "sga", typeToId t, lazyByteString ar, reg idx, reg v]
        GetGlobalArray ty t ar idx -> unwords[ "gga", typeToId ty, reg t, lazyByteString ar, reg idx]

        Call t s f -> unwords ["call", typeToId t, reg s, lazyByteString f]
        Bind t s a -> bla "bind" [typeToId t, reg s, reg a]

        Not s a -> bla "not" [reg s, reg a]

        Function f -> "fun" <> " " <> lazyByteString f

        Label l -> unwords [ "label", label l]
        Jmp l -> unwords [ "jmp", label l]
        JmpT l a -> unwords [ "jmpt", reg a, label l]

        Convert t s t' s' -> unwords [ "conv", typeToId t, reg s, typeToId t', reg s']

        Ret -> "ret"

        Literal t s l -> unwords ["lit", typeToId t, reg s, serializeLit l]



