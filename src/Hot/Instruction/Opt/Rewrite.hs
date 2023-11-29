{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

-- module Hot.Instruction.Opt.Rewrite (rewrite, Rule(..), parse) where
module Hot.Instruction.Opt.Rewrite  where

import Data.Maybe (mapMaybe)
import Data.List (isPrefixOf, foldl')

import qualified Data.Foldable as Foldable


import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

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
            modify' $ Map.insert from to
            return True
        Just to' ->
            if to == to'
            then return True
            else return False

-- this is still the slowest function by far according to profiling
-- it's just called *very* often
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
          case to of
            Reg 0 -> pure False
            _ -> unify from to
        Zero ->
          case to of
            Reg 0 -> pure True
            _ -> pure False
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
    [ Cmd "ccr", Label a, Label b] -> ChangeCodeRef a b

apply :: S -> TokInstruction -> ValInstruction
apply m = map go
  where
    go x = 
      case x of
        Ex y -> y
        Any v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        Temp v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        NotZero v -> Map.findWithDefault  (error $ show x) v m --m Map.! v
        Zero -> Reg 0


data Rule = Rule
  { from :: [ TokInstruction ]
  , to :: [ TokInstruction ]
  } deriving (Show)

type TokInstruction = [Tok]
type ValInstruction = [Value]

data AppliedRule = AppliedRule
  { patterns :: [TokInstruction]
  , state :: S
  , result :: [TokInstruction]
  , originalRule :: Rule
  } deriving (Show)

makeInitialAppliedRule :: Rule -> AppliedRule
makeInitialAppliedRule r@Rule{..} = AppliedRule
  { result = to
  , state = mempty
  , patterns = from
  , originalRule = r
  }

-- this matches/unifies one incoming instruction with one instruction pattern
matchOne :: ValInstruction -> TokInstruction -> M Bool
matchOne ins pattern = match' pattern ins

-- this matches one incoming instruction against a list of possible rules
-- and filters all rules that don't match
matchMultipleRules :: ValInstruction -> [AppliedRule] -> [AppliedRule]
matchMultipleRules ins as = mapMaybe applyOneRule as
  where
    applyOneRule a@AppliedRule{..} =
      case patterns of
        [] -> Just a
        (t:ts) ->
          let (matched, s') = runState (matchOne ins t) state
          in if matched
          then Just $ AppliedRule ts s' result originalRule
          else Nothing

-- matches a list of instructions against a list of possible patterns
matchInstructionSequence :: Foldable t => t ValInstruction -> [AppliedRule] -> [AppliedRule]
matchInstructionSequence instructions ps =
  filter (null . patterns) $ foldl' (flip matchMultipleRules) ps instructions


resultOfAppliedRule :: AppliedRule -> [ValInstruction]
resultOfAppliedRule AppliedRule{..} = map (apply state) result

applyResultToSeq :: Seq ValInstruction -> AppliedRule -> (Seq ValInstruction, Seq ValInstruction)
applyResultToSeq window applied =
  let result = Seq.fromList $ resultOfAppliedRule applied
      leftOver = Seq.drop (length $ from $ originalRule applied) window
  in (result, leftOver)

applyResultToAccumulator :: Accumulator -> AppliedRule -> Accumulator
applyResultToAccumulator acc applied =
  let (result, leftOver) = applyResultToSeq (accWindow acc) applied
  in
     acc {
    accWindow = leftOver
  , accResult = accResult acc Seq.>< result
  }
    

data Accumulator = Accumulator
  { accResult :: Seq ValInstruction
  , accWindow :: Seq ValInstruction
  , accWindowMinSize :: Int
  , accWindowMaxSize :: Int
  , initialRules :: [AppliedRule]
  } deriving (Show)

accOneInstruction :: ValInstruction -> Accumulator -> Accumulator
accOneInstruction ins a@Accumulator{..} =
  let hiWindow@(x Seq.:<| loWindow) = accWindow Seq.|> ins
      a' = a {
          accResult = accResult Seq.|> x
        , accWindow = loWindow
      }
  in if
    -- if the new window fits the requirements run the algorithm on it
    | accWindowMinSize <= Seq.length hiWindow && Seq.length hiWindow <= accWindowMaxSize ->
        case matchInstructionSequence hiWindow initialRules of
          [] -> a {accWindow = hiWindow}
          r:_ -> applyResultToAccumulator (a{ accWindow = hiWindow }) r
    -- if the hiWindow is too large drop the first element and run on the smaller window
    | Seq.length hiWindow > accWindowMaxSize ->
        case matchInstructionSequence loWindow initialRules of
          [] -> a'
          r:_ -> applyResultToAccumulator a' r
    | otherwise -> a { accWindow = hiWindow }

accTail :: Accumulator -> Accumulator
accTail acc@Accumulator{..}
  | Seq.length accWindow < accWindowMinSize =
      acc { accResult = accResult Seq.>< accWindow
          , accWindow = mempty
          }
  | otherwise =
      let x Seq.:<| window' = accWindow
      in case matchInstructionSequence window' initialRules of
        [] -> accTail acc {
            accResult = accResult Seq.|> x
          , accWindow = window'
        }
        r:_ -> accTail $ applyResultToAccumulator acc { accWindow = window', accResult = accResult Seq.|> x } r
  

rewrite :: [Rule] -> [Instruction] -> [Instruction]
rewrite rules ins =
  let inputValues = map toList ins :: [ValInstruction]
      initialAppliedRules = map makeInitialAppliedRule rules
      maxWindowSize = maximum $ map (length.from) rules
      minWindowSize = minimum $ map (length.from) rules

      acc = Accumulator {
          accResult = mempty
        , accWindow = mempty
        , accWindowMinSize = minWindowSize
        , accWindowMaxSize = maxWindowSize
        , initialRules = initialAppliedRules
      }
  in finalizeAccumulator . accTail $ foldl' (flip accOneInstruction) acc inputValues

finalizeAccumulator :: Accumulator -> [Instruction]
finalizeAccumulator Accumulator{..} =
  let t = fmap fromList $ accResult Seq.>< accWindow
  in Foldable.toList t
     

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


