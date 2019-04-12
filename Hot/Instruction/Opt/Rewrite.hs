{-# LANGUAGE OverloadedStrings #-}

module Hot.Instruction.Opt.Rewrite (rewrite, Rule(..), parse, someRules) where

import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf)

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8

import Data.Map (Map)
import qualified Data.Map as Map

import Control.Monad
import Control.Monad.State

import Hot.Instruction hiding (Label)
import Hot.Instruction (Label)
import qualified Hot.Instruction as Hot
import qualified Hot.Types as Hot
import qualified Hot.Ast as Hot
import qualified Hot.Var as Hot

import Debug.Trace


data Tok =
    Any ByteString
  | Temp ByteString
  | Ex Value
  | NotZero ByteString
  deriving (Show)

data Value =
    Reg Register
  | Label Label
  | Type Hot.Type
  | Expr (Hot.Ast Hot.Var Hot.Expr)
  | Cmd ByteString
  | Str String
  deriving (Eq, Show)


type S = Map ByteString Value

type M = State S

unify :: ByteString -> Value -> M Bool
unify from to = do
    e <- Map.lookup from <$> get
    case e of
        Nothing -> do
            modify $ Map.insert from to
            return True
        Just to' ->
            if to == to'
            then return True
            else return False

match' :: [Tok] -> [Value] -> M Bool
match' xs ys = and <$> zipWithM go xs ys
  where
    go from' to =
      case from' of
        Any "*" -> return True
        Any from -> unify from to
        Temp from ->
            if negative to
            then unify from to
            else return False
        NotZero from ->
            if zero to
            then return False
            else unify from to
        Ex from -> return $ from == to

    zero x =
      case x of
        Reg r -> r == 0
        _ -> False
        
    negative x =
      case x of
        Reg r -> r < 0
        _ -> False


toList :: Instruction -> [Value]
toList =
    serializeG Cmd Type Reg Label (pure . Expr) (\n -> [Str n]) (\_ fn -> [Str fn])

fromList :: [Value] -> Instruction
fromList (Cmd x:xs)
  | Just op <- Map.lookup x threeRegs =
    let [Type ty, Reg a, Reg b, Reg c] = xs
    in op ty a b c
  | Just op <- Map.lookup x twoRegs =
    let [Type ty, Reg a, Reg b] = xs
    in op ty a b
  where
    threeRegs = Map.fromList
        [ ("lt", Lt), ("le", Le), ("gt", Gt), ("ge", Ge), ("eq", Eq)
        , ("neq", Neq), ("add", Add), ("sub", Sub), ("mul", Mul), ("div", Div)
        , ("mod", Mod), ("sga", SetGlobalArray), ("gga", GetGlobalArray)
        , ("sla", SetLocalArray), ("gla", GetLocalArray)
        ]
    twoRegs = Map.fromList
        [ ("neg", Negate), ("set", Set), ("sg", SetGlobal), ("gg", GetGlobal)
        , ("bind", Bind)
        ]
fromList x =
  case x of
    [ Cmd "lit", Type ty, Reg r, Expr v] -> Literal ty r v
    [ Cmd "call", Reg r, Label l, Str n] -> Call r l n
    [ Cmd "conv", Type t, Reg a, Type s, Reg b] -> Convert t a s b
    [ Cmd "label", Label l] -> Hot.Label l
    [ Cmd "jmp", Label l] -> Jmp l
    [ Cmd "fun", Label l, Str n] -> Function l n
    [ Cmd "jmpt", Label l, Reg r] -> JmpT l r
    [ Cmd "not", Reg a, Reg b] -> Not a b
    [ Cmd "ret", Type ty] -> Ret ty

apply :: S -> [Tok] -> [Value]
apply m = map go
  where
    go x = 
      case x of
        Ex y -> y
        Any v -> m Map.! v
        Temp v -> m Map.! v


match :: [ [Value] ] -> [ [Tok] ] -> (Bool, S)
match v t = flip runState mempty $ and <$> zipWithM match' t v

matchRule :: [[Value]] -> Rule -> Maybe (S, Rule)
matchRule v r = --trace (unwords ["Trying to match", show v, "against rule", show r]) $
  case match v (from r) of
    (False, _) -> Nothing --trace "Did not match" Nothing
    (True, s) -> Just (s, r) --trace "matched" $ Just (s, r)

data Rule = Rule
  { from :: [ [Tok] ]
  , to :: [ [Tok] ]
  } deriving (Show)
  
rewrite :: [Rule] -> [Instruction] -> [Instruction]
rewrite rules input = go mempty input'
  where
    input' :: [[Value]]
    input' = map toList input
    
    go :: [[Value]] -> [[Value]] -> [Instruction]
    go acc [] = map fromList acc
    go acc ls@(x:xs) =
        let potentialMatches = mapMaybe (matchRule ls) rules
        in case potentialMatches of
            (s, r):_ -> --trace (unwords ["matched", show r, "with", show s ]) $
                let ls' :: [[Value]]
                    ls' = drop (length $ from r) ls
                    new = map (apply s) $ to r
                in go acc $ {-trace (unwords ["continuing with", show $ new <> ls']) $-} new <> ls'
            [] -> go (acc `snoc` x) xs
            
    snoc xs x = xs ++ [x]


parse :: String -> [Tok]
parse = map p . words
  where
    p t
      | Just _ <- Map.lookup t Hot.types = Ex $ Type t
      | Just _ <- lookup t Hot.instable = Ex . Cmd $ L8.pack t
      | "%" `isPrefixOf` t = Temp $ L8.pack t
      | "#" `isPrefixOf` t = NotZero $ L8.pack t
      -- TODO: jass expressions
      | otherwise = Any $ L8.pack t


mkRR is os = Rule (map parse is) (map parse os)

eliminateTmpThreeArg =
    [ mkRR
        [ cmd ++ " type %r a b"
        , "set type t %r"
        ]
        [ cmd ++ " type t a b"]
        
    | cmd <- ["add", "sub", "mul", "div", "mod", "lt", "le", "gt", "ge", "neq", "eq", "gla", "gga"]
    ]


eliminateLocalCompBeforeRet =
  [ mkRR
    [ cmd ++ " * * #z *"
    , "ret t"
    ]
    [ "ret t" ]
  | cmd <- ["add", "sub", "mul", "div", "mod", "lt", "le", "gt", "ge", "neq", "eq", "gla", "gga"]
  ]

eliminateLocalSetBeforeRet =
  [ mkRR
    [ "set * #z *"
    , "ret t"
    ]
    [ "ret t" ]
  , mkRR
    [ "lit * #z *"
    , "ret t"
    ]
    [ "ret t" ]
  , mkRR
    [ "gg * #z *"
    , "ret t"
    ]
    [ "ret t" ]
  , mkRR
    [ "sla * * * *"
    , "ret t"
    ]
    [ "ret t" ]
  ]

eliminateTmpTwoArg =
  [ mkRR
    [ "neg type %r a"
    , "set type v %r"
    ]
    [ "neg v a" ]
  , mkRR
    [ "set type %r a"
    , "set type b %r"
    ]
    [ "set type b a" ]
  , mkRR
    [ "gg type %r g"
    , "set type l %r"
    ]
    [ "gg type l g" ]
  , mkRR
    [ "lit type %r v"
    , "set type l %r"
    ]
    [ "lit type l v"]
  ]

eliminateUselessJmp =
  [ mkRR
    [ "jmp lbl"
    , "label lbl"
    ]
    [ "label lbl" ]
  , mkRR
    [ "jmpt lbl r"
    , "label lbl"
    ]
    [ "label lbl" ]
  ]

eliminateTmpCall =
  [ mkRR
    [ "call %t fn n"
    , "set type a %t"
    ]
    [ "call a fn n" ]
  ]
  
someRules =
    eliminateUselessJmp
    <> eliminateTmpTwoArg
    <> eliminateLocalSetBeforeRet
    <> eliminateLocalCompBeforeRet
    <> eliminateTmpThreeArg
    <> eliminateTmpCall