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


(-->) is os = Rule (map parse is) (map parse os)

eliminateTmpComp =
  [ [ cmd ++ " type %r a b"
    , "set type t %r"
    ] -->
    [ cmd ++ " type t a b"]
      
  | cmd <- ["add", "sub", "mul", "div", "mod", "gla", "gga"]
  ]

eliminateTmpCompBool =
  [ [ cmd ++ " type %r a b"
    , "set boolean t %r"
    ] -->
    [ cmd ++ " type t a b"]
  | cmd <- ["lt", "le", "gt", "ge", "neq", "eq"]
  ]


eliminateLocalCompBeforeRet =
  [ [ cmd ++ " * #z * *"
    , "ret t"
    ] --> 
    [ "ret t" ]
  | cmd <- ["add", "sub", "mul", "div", "mod", "lt", "le", "gt", "ge", "neq", "eq", "gla", "gga"]
  ]

eliminateLocalSetBeforeRet =
  [ [ "set * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "lit * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "gg * #z *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "sla * * * *"
    , "ret t"
    ] -->
    [ "ret t" ]
    
  , [ "call t fn n"
    , "set * s t "
    , "ret type"
    ] -->
    [ "call s fn n"
    , "ret type"
    ]
  ]

eliminateTmpOther =
  [ [ "neg type %r a"
    , "set type v %r"
    ] -->
    [ "neg type v a" ]
    
  , [ "set type %r a"
    , "set type b %r"
    ] -->
    [ "set type b a" ]
    
  , [ "gg type %r g"
    , "set type l %r"
    ] -->
    [ "gg type l g" ]
    
  , [ "lit type %r v"
    , "set type l %r"
    ] -->
    [ "lit type l v"]
  
  , [ "conv t1 %r t2 s"
    , "set t1 t %r"
    ] -->
    [ "conv t1 t t2 s" ]
  
  ]

eliminateUselessJmp =
  [ [ "jmp lbl"
    , "label lbl"
    ] -->
    [ "label lbl" ]
    
  , [ "jmpt lbl r"
    , "label lbl"
    ] -->
    [ "label lbl" ]
  ]

eliminateTmpCall =
  [ [ "call %t fn n"
    , "set * a %t"
    ] -->
    [ "call a fn n" ]
  ]

compG =
  [ [ "lt type t a b"
    , "not t t"
    ] -->
    [ "ge type t a b"]
  
  , [ "le type t a b"
    , "not t t"
    ] -->
    [ "gt type t a b"]
  
  , [ "gt type t a b"
    , "not t t"
    ] -->
    [ "le type t a b"]
  
  , [ "ge type t a b"
    , "not t t"
    ] -->
    [ "lt type t a b"]
  ]

-- unfortunately wc3 handles real (==) differently than not (!=)
compNotReal = eq <> neq
  where
    types = filter (`notElem`["real", "nothing"]) $ Map.keys Hot.types
    eq =
      [ [ "eq " <> ty <> " t a b"
        , "not t t"
        ] -->
        [ "neq " <> ty <> " t a b"]
      | ty <- types
      ]
    neq =
      [ [ "neq " <> ty <> " t a b"
        , "not t t"
        ] -->
        [ "eq " <> ty <> " t a b"]
      | ty <- types
      ]
  

algebraic =
  [
    -- distributive law for integers
    -- probably never fires in real code...
    [ "mul integer %t a b"
    , "mul integer %s a c"
    , "add integer d %t %s"
    ] -->
    [ "add integer %t b c"
    , "mul integer d a %t"
    ]
    -- mul by zero for both integer and real
  , [ "lit integer %z i'0"
    , "mul integer t a %z"
    ] -->
    [ "lit integer t i'0"]
  
  , [ "lit integer %z i'0"
    , "mul integer t %z a"
    ] -->
    [ "lit integer t i'0"]
    
  , [ "lit real %z r'0"
    , "mul real t a %z"
    ] -->
    [ "lit real t r'0"]
  
  , [ "lit real %z r'0"
    , "mul real t %z a"
    ] -->
    [ "lit real t r'0"]
  
  -- mul by one for both integer and real
  , [ "lit integer %z i'1"
    , "mul integer t a %z"
    ] -->
    [ "set integer t a"]
  
  , [ "lit integer %z i'1"
    , "mul integer t %z a"
    ] -->
    [ "set integer t a"]
    
  , [ "lit real %z r'1"
    , "mul real t a %z"
    ] -->
    [ "set real t a"]
  
  , [ "lit real %z r'1"
    , "mul real t %z a"
    ] -->
    [ "set real t a"]
  
  -- add by zero for both integer and real
  , [ "lit integer %z i'0"
    , "add integer t a %z"
    ] -->
    [ "set integer t a"]
    
  , [ "lit integer %z i'0"
    , "add integer t %z a"
    ] -->
    [ "set integer t a"]
  
  , [ "lit real %z r'0"
    , "add real t a %z"
    ] -->
    [ "set real t a"]
    
  , [ "lit real %z r'0"
    , "add real t %z a"
    ] -->
    [ "set real t a"]
  ]

removeComputeToZero =
  [ [ cmd <> " type t a b"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ cmd <> " type 0 a b"
    , "ret type"
    ]
  | cmd <- ["add", "sub", "mul", "div", "mod", "gla", "gga"]
  ]

removeCompareToZero =
  [ [ cmd <> " type t a b"
    , "set boolean 0 t"
    , "ret boolean"
    ] -->
    [ cmd <> " type 0 a b"
    , "ret boolean"
    ]
  | cmd <- ["lt", "le", "gt", "ge", "neq", "eq"]
  ]


removeSetToZero =
  [ [ "set type t a"
    , "set type 0 t"
    , "ret type"
    ] -->
    [ "set type 0 a"
    , "ret type"
    ]
  ]


-- everything but label and fun after ret is removed
removeUnreachableCode =
     args "ret type" 1 ["jmp", "ret"]
  <> args "ret type" 2 ["jmpt", "not"]
  <> args "ret type" 3 ["call", "lit", "neg", "set", "gg", "sg", "bind"]
  <> args "ret type" 4 ["lt", "le", "gt", "ge", "eq", "neq"]
  <> args "ret type" 4 ["div", "mul", "mod", "add", "sub"]
  <> args "ret type" 4 ["sga", "gga", "sla", "gla", "conv"]

removeEmptyJumps =
  [ [ "jmpt l1 cond"
    , "jmp l2"
    , "label l1"
    , "label l2"
    ] -->
    [ "label l1" -- just keep the labels for good measure
    , "label l2"
    ]
  , [ "lit boolean t true"
    , "jmpt lbl t"
    ] -->
    [ "lit boolean t true"
    , "jmp lbl"
    ]
  , [ "lit boolean t false"
    , "jmpt lbl t"
    ] -->
    [ "lit boolean t false"]
  ]

removeCodeAfterJump =
     args "jmp lbl" 1 ["jmp", "ret"]
  <> args "jmp lbl" 2 ["jmpt", "not"]
  <> args "jmp lbl" 3 ["call", "lit", "neg", "set", "gg", "sg", "bind"]
  <> args "jmp lbl" 4 ["lt", "le", "gt", "ge", "eq", "neq"]
  <> args "jmp lbl" 4 ["div", "mul", "mod", "add", "sub"]
  <> args "jmp lbl" 4 ["sga", "gga", "sla", "gla", "conv"]

-- used by removeCodeAfterJump, removeUnreachableCode
args b n = map (mkRule n)
  where
    mkRule n m =
        let cmd = unwords $ m:replicate n "*"
        in [b, cmd] --> [b]


someRules =
       eliminateUselessJmp
    <> eliminateTmpOther
    <> eliminateLocalSetBeforeRet
    <> eliminateLocalCompBeforeRet
    <> eliminateTmpComp
    <> eliminateTmpCall
    <> eliminateTmpCompBool
    <> compNotReal
    <> compG
    <> algebraic
    <> removeComputeToZero
    <> removeSetToZero
    <> removeCompareToZero
    <> removeUnreachableCode
    <> removeEmptyJumps
    <> removeCodeAfterJump