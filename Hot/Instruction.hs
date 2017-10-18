
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

type Register = Int32
type Label = Int32


data Instruction
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
    | Negate Type Register Register

    | Literal Type Register (Hot.Ast Var Expr)
    
    | Set Type Register Register
    | SetArray Type Register Register Register
    | GetArray Type Register Register Register
    | SetGlobal Type Register Register
    | GetGlobal Type Register Register
    | SetGlobalArray Type Register Register Register
    | GetGlobalArray Type Register Register Register

    | Call Type Register Register
    | Bind Type Register Register

    | Not Register Register

    | Label Register
    | Jmp Register
    | JmpT Register Register

    | Convert Type Register Type Register

    | Function Label
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

    bla ins args = unwords [ins, unwords $ map int32Dec args]

    typeToId = (Hot.types Map.! )

    s ins =
      case ins of
        Lt t s a b -> bla "lt" [typeToId t, s, a, b]
        Le t s a b -> bla "le" [typeToId t, s, a, b]
        Gt t s a b -> bla "gt" [typeToId t, s, a, b]
        Ge t s a b -> bla "ge" [typeToId t, s, a, b]
        Eq t s a b -> bla "eq" [typeToId t, s, a, b]
        Neq t s a b -> bla "neq" [typeToId t, s, a, b]
        Add t s a b -> bla "add" [typeToId t, s, a, b]
        Sub t s a b -> bla "sub" [typeToId t, s, a, b]
        Mul t s a b -> bla "mul" [typeToId t, s, a, b]
        Div t s a b -> bla "div" [typeToId t, s, a, b]

        Negate t s a -> bla "neg" [typeToId t, s, a]

        Set t s a -> bla "set" [typeToId t, s, a]
        SetGlobal t s a -> bla "setglobal" [typeToId t, s, a]
        GetGlobal t s a -> bla "getglobal" [typeToId t, s, a]

        SetArray ty ar idx r -> bla "setarray" [typeToId ty, ar, idx, r]
        GetArray ty t ar idx -> bla "getarray" [typeToId ty, t, ar, idx]

        SetGlobalArray t ar idx v -> bla "setglobalarray" [typeToId t, ar, idx, v]
        GetGlobalArray ty t ar idx -> bla "getglobalarray" [typeToId ty, t, ar, idx]

        Call t s f -> bla "call" [typeToId t, s, f]
        Bind t s a -> bla "bind" [typeToId t, s, a]

        Not s a -> bla "not" [s, a]

        Function f -> bla "fun" [f]

        Label l -> bla "label" [l]
        Jmp l -> bla "jmp" [l]
        JmpT l a-> bla "jmpt" [l, a]

        Convert t s t' s' -> bla "conv" [typeToId t, s, typeToId t', s']

        Ret -> bla "ret" []

        Literal t s l -> unwords [string8 "lit", int32Dec $ typeToId t, int32Dec s, serializeLit l]



