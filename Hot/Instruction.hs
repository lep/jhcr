{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Hot.Instruction
    ( Instruction (..)
    , Register, Label
    , serialize, serializeAsm, serializeChunked
    , serializeG
    , instable
    ) where

import Data.List (intersperse, foldl', genericLength)
import Data.Monoid
import Data.Int
import Data.Maybe
import Data.String

import qualified Data.Map as Map

import Text.Printf (printf)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString.Builder

import Hot.Ast (Expr, Name, Type, Var(..))
import qualified Hot.Ast as Hot
import qualified Hot.Types as Hot


newtype Register = Register Int32
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

reg :: Register -> Builder
reg (Register r) = pad9Dec r

reg' :: Register -> Builder
reg' (Register r) = int32Dec r


newtype Label = Lbl Int16
    deriving (Eq, Ord, Show, Enum, Real, Integral, Num)

label :: Label -> Builder
label (Lbl l) = pad6Dec l

label' :: Label -> Builder
label' (Lbl l) = int16Dec l

data Instruction
    -- three regs
    = Lt                Type Register Register Register
    | Le                Type Register Register Register
    | Gt                Type Register Register Register
    | Ge                Type Register Register Register
    | Eq                Type Register Register Register
    | Neq               Type Register Register Register
    | Add               Type Register Register Register
    | Sub               Type Register Register Register
    | Mul               Type Register Register Register
    | Div               Type Register Register Register
    
    -- Mod only works on integers but for ease we still include the Type
    | Mod               Type Register Register Register

    | SetGlobalArray    Type Register Register Register
    | GetGlobalArray    Type Register Register Register
    | SetLocalArray     Type Register Register Register
    | GetLocalArray     Type Register Register Register
    
    -- two regs
    | Negate    Type Register Register
    | Set       Type Register Register
    | SetGlobal Type Register Register
    | GetGlobal Type Register Register
    | Bind      Type Register Register

    -- special
    | Literal Type Register (Hot.Ast Var Expr) -- encoded as: lit ty reg len string
    | Call Register Label Name
    | Convert Type Register Type Register

    -- one label
    | Label Label
    | Jmp Label
    | Function Label Name
    
    | JmpT Label Register
    
    | Not Register Register

    | Ret Type
    deriving (Show)

pad2Dec :: Int16 -> Builder
pad2Dec x =
  let l = intlog10 $ abs x
      w = 2 - if x < 0 then 1 else 0
  in int16Dec x <> stringUtf8 (replicate (w-l) '.')

pad3Dec :: Int16 -> Builder
pad3Dec x =
  let l = intlog10 $ abs x
      w = 3 - if x < 0 then 1 else 0
  in int16Dec x <> stringUtf8 (replicate (w-l) '.')


pad6Dec :: Int16 -> Builder
pad6Dec x =
  let l = intlog10 $ abs x
      w = 6 - if x < 0 then 1 else 0
  in int16Dec x <> stringUtf8 (replicate (w-l) '.')

pad9Dec :: Int32 -> Builder
pad9Dec x =
  let l = intlog10 $ abs x
      w = 9 - if x < 0 then 1 else 0
  in int32Dec x <> stringUtf8 (replicate (w-l) '.')


intlog10 :: (Integral a, Num b) => a -> b
intlog10 = fromIntegral . log10 . fromIntegral
  where
    log10 :: Integer -> Integer
    log10 0 = 1
    log10 x = fst . head . filter ( (x <) . snd ) $ zip [0..] (iterate (*10) 1)


serializeLit :: Hot.Ast Var Expr -> Builder
serializeLit l =
  case l of
    Hot.Int i -> int32Dec i
    Hot.Real r -> stringUtf8 $ printf "%f" r
    Hot.String s -> stringUtf8 s
    Hot.Bool True -> stringUtf8 "true"
    Hot.Bool False -> stringUtf8 "false"
    Hot.Null -> stringUtf8 "null"

serializeG :: (IsString s)
           => (s -> a)
           -> (Type -> a)
           -> (Register -> a)
           -> (Label -> a)
           -> (Hot.Ast Var Expr -> [a])
           -> (Name -> [a])
           -> (Label -> Name -> [a])
           -> (Instruction -> [a])
serializeG op ty reg lbl expr call fn ins =
  case ins of
    Lt t s a b -> [ op "lt", ty t, reg s, reg a, reg b]
    Le t s a b -> [ op "le", ty t, reg s, reg a, reg b]
    Gt t s a b -> [ op "gt", ty t, reg s, reg a, reg b]
    Ge t s a b -> [ op "ge", ty t, reg s, reg a, reg b]
    Eq t s a b -> [ op "eq", ty t, reg s, reg a, reg b]
    Neq t s a b -> [ op "neq", ty t, reg s, reg a, reg b]
    Add t s a b -> [ op "add", ty t, reg s, reg a, reg b]
    Sub t s a b -> [ op "sub", ty t, reg s, reg a, reg b]
    Mul t s a b -> [ op "mul", ty t, reg s, reg a, reg b]
    Div t s a b -> [ op "div", ty t, reg s, reg a, reg b]
    Mod t s a b -> [ op "mod", ty t, reg s, reg a, reg b]

    Negate t s a ->  [ op "neg", ty t, reg s, reg a]

    Set t s a -> [ op "set", ty t, reg s, reg a]
    SetGlobal tyv g s -> [ op "sg", ty tyv, reg g, reg s]
    GetGlobal tyv s g -> [ op "gg", ty tyv, reg s, reg g]

    SetLocalArray tyv ar idx r -> [ op "sla", ty tyv, reg ar, reg idx, reg r]
    GetLocalArray tyv t ar idx -> [ op "gla", ty tyv, reg t, reg ar, reg idx]

    SetGlobalArray tyv ar idx v -> [ op "sga", ty tyv, reg ar, reg idx, reg v]
    GetGlobalArray tyv t ar idx -> [ op "gga", ty tyv, reg t, reg ar, reg idx]

    Call s f n -> [ op "call", reg s, lbl f] <> call n
    Bind t s a -> [ op "bind", ty t, reg s, reg a]

    Not s a -> [ op "not", reg s, reg a]

    Function f n -> [ op "fun", lbl f] <> fn f n

    Label l -> [ op "label", lbl l]
    Jmp l -> [ op "jmp", lbl l]
    JmpT l a -> [ op "jmpt", lbl l, reg a]

    Convert t s t' s' -> [ op "conv", ty t, reg s, ty t', reg s']

    Ret tyv -> [ op "ret", ty tyv ]

    Literal t s l -> [ op "lit", ty t, reg s ] <> expr l


serializeAsm :: [Instruction] -> Builder
serializeAsm = unlines . map (unwords . s)
  where
    s = serializeG id stringUtf8 reg' label' (pure . serializeLit) (pure . stringUtf8) (\_ n -> [stringUtf8 n])
    unlines = mconcat . intersperse (charUtf8 '\n')
    unwords = mconcat . intersperse (charUtf8 ' ')

serialize' :: Instruction -> Builder
serialize' =
    mconcat . serializeG ins2id typeToId reg label sLit (const mempty) sFn
  where
    sFn f n =
        if f < 0
        then [pad6Dec (genericLength n), stringUtf8 n]
        else []
    sLit l =
        let litRendered = serializeLit l
            litLen = fromIntegral . BL.length $ toLazyByteString litRendered
        in [ pad6Dec litLen, litRendered ]
    typeToId x = pad3Dec (Map.findWithDefault (error x) x Hot.types)
    ins2id n = pad2Dec . fromMaybe (error $ "unknown op" <> show n) $ lookup n instable
    
instable =
      [ ("lt", 1), ("le", 2), ("gt", 3), ("ge", 4), ("eq", 5), ("neq", 6)
      , ("add", 7), ("sub", 8), ("mul", 9), ("div", 10), ("mod", 11)
      , ("sla", 12), ("gla", 13), ("sga", 14), ("gga", 15), ("neg", 16)
      , ("set", 17), ("sg", 18), ("gg", 19), ("bind", 20), ("lit", 21)
      , ("call", 22), ("conv", 23), ("label", 24), ("jmp", 25), ("fun", 26)
      , ("jmpt", 27), ("not", 28), ("ret", 29)
      ]
    

serialize :: [Instruction] -> Builder
serialize = mconcat . map serialize'

serializeChunked :: Int64 -> [Instruction] -> [String]
serializeChunked chunkSize =
    map L8.unpack .
    map toLazyByteString .
    map fst .
    foldl' go [] .
    map (\x -> (lazyByteString x, Sum $ BL.length x)) .
    map toLazyByteString . 
    map serialize'
  where
    go [] elem = [elem]
    go (x:xs) elem =
        let x' = x <> elem
        in if getSum (snd x') > chunkSize
        then elem:x:xs
        else x':xs

