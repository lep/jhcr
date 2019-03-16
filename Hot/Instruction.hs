
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}

module Hot.Instruction
    ( Instruction (..)
    , Register, Label
    , serialize, serializeAsm
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

newtype Register = Register Int32
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

reg :: Register -> Builder
reg (Register r) = pad32Dec r

reg' :: Register -> Builder
reg' (Register r) = int32Dec r


newtype Label = Lbl Int16
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

label :: Label -> Builder
label (Lbl l) = pad16Dec l

label' :: Label -> Builder
label' (Lbl l) = int16Dec l

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
    | Mod Type Register Register Register -- Mod only works on integers but for ease we still include the Type

    | SetGlobalArray Type Register Register Register
    | GetGlobalArray Type Register Register Register
    | SetLocalArray Type Register Register Register
    | GetLocalArray Type Register Register Register
    
    -- two regs
    | Negate Type Register Register
    | Set Type Register Register
    | SetGlobal Type Register Register
    | GetGlobal Type Register Register
    | Bind Type Register Register

    -- special
    | Literal Type Register (Hot.Ast Var Expr) -- encoded as: lit ty reg len string
    | Call Type Register Label Name
    | Convert Type Register Type Register

    -- one label
    | Label Label
    | Jmp Label
    | Function Label Name
    
    | JmpT Label Register --encoded as: jmpt reg label
    
    | Not Register Register

    
    | Ret
    deriving (Show)

pad5Dec :: Int8 -> Builder
pad5Dec x =
  let l = intlog10 $ abs x
      w = 2 - if x < 0 then 1 else 0
  in int8Dec x <> stringUtf8 (replicate (w-l) '.')

pad7Dec :: Int8 -> Builder
pad7Dec x =
  let l = intlog10 $ abs x
      w = 3 - if x < 0 then 1 else 0
  in int8Dec x <> stringUtf8 (replicate (w-l) '.')

pad8Dec :: Int8 -> Builder
pad8Dec x =
  let l = intlog10 $ abs x
      w = 4 - if x < 0 then 1 else 0
  in int8Dec x <> stringUtf8 (replicate (w-l) '.')


pad16Dec :: Int16 -> Builder
pad16Dec x =
  let l = intlog10 $ abs x
      w = 6 - if x < 0 then 1 else 0
  in int16Dec x <> stringUtf8 (replicate (w-l) '.')

pad32Dec :: Int32 -> Builder
pad32Dec x =
  let l = intlog10 $ abs x
      w = 11 - if x < 0 then 1 else 0
  in int32Dec x <> stringUtf8 (replicate (w-l) '.')


intlog10 :: (Integral a, Num b) => a -> b
intlog10 = fromIntegral . log10 . fromIntegral
  where
    log10 :: Integer -> Integer
    log10 0 = 1
    log10 x = fst . head . filter ( (x <) . snd ) $ zip [0..] (iterate (*10) 1)

serializeAsm :: [Instruction] -> Builder
serializeAsm = unlines . map s
  where
    unlines = mconcat . intersperse (charUtf8 '\n')
    unwords = mconcat . intersperse (charUtf8 ' ')

    serializeLit :: Hot.Ast Var Expr -> Builder
    serializeLit l =
      case l of
        Hot.Int i -> int32Dec i
        Hot.Real r -> floatDec r
        Hot.String s -> stringUtf8 s
        Hot.Bool s -> stringUtf8 $ show s
        Hot.Null -> stringUtf8 "null"

    bla ins args = unwords [ins, unwords args]

    typeToId x = stringUtf8 x

    ins2id = id

    s ins =
      case ins of
        Lt t s a b -> unwords [ ins2id "lt", typeToId t, reg' s, reg' a, reg' b]
        Le t s a b -> unwords [ ins2id "le", typeToId t, reg' s, reg' a, reg' b]
        Gt t s a b -> unwords [ ins2id "gt", typeToId t, reg' s, reg' a, reg' b]
        Ge t s a b -> unwords [ ins2id "ge", typeToId t, reg' s, reg' a, reg' b]
        Eq t s a b -> unwords [ ins2id "eq", typeToId t, reg' s, reg' a, reg' b]
        Neq t s a b -> unwords [ ins2id "neq", typeToId t, reg' s, reg' a, reg' b]
        Add t s a b -> unwords [ ins2id "add", typeToId t, reg' s, reg' a, reg' b]
        Sub t s a b -> unwords [ ins2id "sub", typeToId t, reg' s, reg' a, reg' b]
        Mul t s a b -> unwords [ ins2id "mul", typeToId t, reg' s, reg' a, reg' b]
        Div t s a b -> unwords [ ins2id "div", typeToId t, reg' s, reg' a, reg' b]
        Mod t s a b -> unwords [ ins2id "mod", typeToId t, reg' s, reg' a, reg' b]

        Negate t s a -> bla (ins2id "neg") [typeToId t, reg' s, reg' a]

        Set t s a -> unwords [ ins2id "set", typeToId t, reg' s, reg' a]
        SetGlobal ty g s -> unwords [ ins2id "sg", typeToId ty, reg' g, reg' s]
        GetGlobal ty s g -> unwords [ ins2id "gg", typeToId ty, reg' s, reg' g]

        SetLocalArray ty ar idx r -> unwords [ ins2id "sla", typeToId ty, reg' ar, reg' idx, reg' r]
        GetLocalArray ty t ar idx -> unwords [ ins2id "gla", typeToId ty, reg' t, reg' ar, reg' idx]

        SetGlobalArray t ar idx v -> unwords [ ins2id "sga", typeToId t, reg' ar, reg' idx, reg' v]
        GetGlobalArray ty t ar idx -> unwords[ ins2id "gga", typeToId ty, reg' t, reg' ar, reg' idx]

        Call t s f n -> unwords [ins2id "call", typeToId t, reg' s, label' f, stringUtf8 n]
        Bind t s a -> bla (ins2id "bind") [typeToId t, reg' s, reg' a]

        Not s a -> bla (ins2id "not") [reg' s, reg' a]

        Function f n -> unwords [ins2id "fun", label' f, stringUtf8 n]

        Label l -> unwords [ ins2id "label", label' l]
        Jmp l -> unwords [ ins2id "jmp", label' l]
        JmpT l a -> unwords [ ins2id "jmpt", reg' a, label' l]

        Convert t s t' s' -> unwords [ ins2id "conv", typeToId t, reg' s, typeToId t', reg' s']

        Ret -> ins2id "ret"

        Literal t s l ->
            let litRendered = serializeLit l
                litLen = fromIntegral . BL.length $ toLazyByteString litRendered
            in unwords [ins2id "lit", typeToId t, reg' s, int16Dec litLen, litRendered]
  
serialize :: [Instruction] -> Builder
serialize = unlines . map s
  where
    unlines = mconcat {-. intersperse (charUtf8 '\n')-}
    unwords = mconcat {-. intersperse (charUtf8 ' ')-}

    serializeLit :: Hot.Ast Var Expr -> Builder
    serializeLit l =
      case l of
        Hot.Int i -> int32Dec i
        Hot.Real r -> floatDec r
        Hot.String s -> stringUtf8 s
        Hot.Bool s -> stringUtf8 $ show s
        Hot.Null -> stringUtf8 "null"

    bla ins args = unwords [ins, unwords args]

    typeToId x = pad7Dec (Map.findWithDefault (error $  x) x Hot.types) --Hot.types Map.! x)
    --typeToId x = stringUtf8   x
    
    --ins2id = id
    ins2id n = pad5Dec . fromMaybe (error $ "unknown op" <> show n) $ lookup n instable
    
    instable =
      [ ("lt", 1), ("le", 2), ("gt", 3), ("ge", 4), ("eq", 5), ("neq", 6)
      , ("add", 7), ("sub", 8), ("mul", 9), ("div", 10), ("mod", 11)
      , ("sla", 12), ("gla", 13), ("sga", 14), ("gga", 15), ("neg", 16)
      , ("set", 17), ("sg", 18), ("gg", 19), ("bind", 20), ("lit", 21)
      , ("call", 22), ("conv", 23), ("label", 24), ("jmp", 25), ("fun", 26)
      , ("jmpt", 27), ("not", 28), ("ret", 29)
      ]

    s ins =
      case ins of
        Lt t s a b -> unwords [ ins2id "lt", typeToId t, reg s, reg a, reg b]
        Le t s a b -> unwords [ ins2id "le", typeToId t, reg s, reg a, reg b]
        Gt t s a b -> unwords [ ins2id "gt", typeToId t, reg s, reg a, reg b]
        Ge t s a b -> unwords [ ins2id "ge", typeToId t, reg s, reg a, reg b]
        Eq t s a b -> unwords [ ins2id "eq", typeToId t, reg s, reg a, reg b]
        Neq t s a b -> unwords [ ins2id "neq", typeToId t, reg s, reg a, reg b]
        Add t s a b -> unwords [ ins2id "add", typeToId t, reg s, reg a, reg b]
        Sub t s a b -> unwords [ ins2id "sub", typeToId t, reg s, reg a, reg b]
        Mul t s a b -> unwords [ ins2id "mul", typeToId t, reg s, reg a, reg b]
        Div t s a b -> unwords [ ins2id "div", typeToId t, reg s, reg a, reg b]
        Mod t s a b -> unwords [ ins2id "mod", typeToId t, reg s, reg a, reg b]

        Negate t s a -> bla (ins2id "neg") [typeToId t, reg s, reg a]

        Set t s a -> unwords [ ins2id "set", typeToId t, reg s, reg a]
        SetGlobal ty g s -> unwords [ ins2id "sg", typeToId ty, reg g, reg s]
        GetGlobal ty s g -> unwords [ ins2id "gg", typeToId ty, reg s, reg g]

        SetLocalArray ty ar idx r -> unwords [ ins2id "sla", typeToId ty, reg ar, reg idx, reg r]
        GetLocalArray ty t ar idx -> unwords [ ins2id "gla", typeToId ty, reg t, reg ar, reg idx]

        SetGlobalArray t ar idx v -> unwords [ ins2id "sga", typeToId t, reg ar, reg idx, reg v]
        GetGlobalArray ty t ar idx -> unwords[ ins2id "gga", typeToId ty, reg t, reg ar, reg idx]

        Call t s f _ -> unwords [ins2id "call", typeToId t, reg s, label f]
        Bind t s a -> bla (ins2id "bind") [typeToId t, reg s, reg a]

        Not s a -> bla (ins2id "not") [reg s, reg a]

        Function f _ -> unwords [ins2id "fun", label f]

        Label l -> unwords [ ins2id "label", label l]
        Jmp l -> unwords [ ins2id "jmp", label l]
        JmpT l a -> unwords [ ins2id "jmpt", reg a, label l]

        Convert t s t' s' -> unwords [ ins2id "conv", typeToId t, reg s, typeToId t', reg s']

        Ret -> ins2id "ret"

        Literal t s l ->
            let litRendered = serializeLit l
                litLen = fromIntegral . BL.length $ toLazyByteString litRendered
            in unwords [ins2id "lit", typeToId t, reg s, pad16Dec litLen, litRendered]


