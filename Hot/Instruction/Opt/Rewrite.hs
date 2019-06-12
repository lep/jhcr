{-# LANGUAGE OverloadedStrings #-}

module Hot.Instruction.Opt.Rewrite (rewrite, Rule(..), parse) where

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

data Tok =
    Any ByteString
  | Temp ByteString
  | Ex Value
  | NotZero ByteString
  | Zero
  | Wildcard
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
    e <- gets $ Map.lookup from
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
        Wildcard -> return True
        Any from -> unify from to
        Temp from ->
            if negative to
            then unify from to
            else return False
        NotZero from ->
            if zero to
            then return False
            else unify from to
        Zero ->
            if zero to
            then return True
            else return False
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
        Any v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        Temp v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        NotZero v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        Zero -> Reg 0


match :: [ [Value] ] -> [ [Tok] ] -> (Bool, S)
match [] _ = (False, mempty)
match v t =
  let v' = take (length t) v
  in if length v < length t
  then (False, mempty)
  else flip runState mempty $ and <$> zipWithM match' t v'

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
rewrite _ [] = []
rewrite rules input = {-trace (unwords ["rewriting", show input]) $-} go mempty input'
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
      | t == "*" = Wildcard
      | t == "0" = Zero
      | t == "null" = Ex . Expr $ Hot.Null
      | t == "true" = Ex . Expr $ Hot.Bool True
      | t == "false" = Ex . Expr $ Hot.Bool False
      | "r'" `isPrefixOf` t = Ex . Expr . Hot.Real . read $ drop 2 t
      | "i'" `isPrefixOf` t = Ex . Expr . Hot.Int . read $ drop 2 t
      | otherwise = Any $ L8.pack t


