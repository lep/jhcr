


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}


import Jass.Ast
import Data.Composeable

import Control.Arrow (second)

import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.String (fromString)


replaceExecuteFunc :: Ast Name a -> Ast Name a
replaceExecuteFunc e =
  case e of
    Call "ExecuteFunc" args -> Call "JHCR_ExecuteFunc" args
    _ -> composeOp replaceExecuteFunc e

replaceCodeLiterals :: Ast Name a -> Ast Name a
replaceCodeLiterals e =
  case e of
    Code c -> Code $ "JHCR_" <> c <> "_stub"
    _ -> composeOp replaceCodeLiterals e


newtype SetMap = SetMap {getMap :: Map (Type, Bool) (Set Name)}

singleton typ isArray name = SetMap $ Map.singleton (typ, isArray) $ Set.singleton name

instance Monoid SetMap where
    mempty = SetMap mempty
    SetMap a `mappend` SetMap b = SetMap $ Map.unionWith Set.union a b

gatherGlobals :: Ast Name a -> Map (Type, Bool) (Set Name)
gatherGlobals = getMap . go
  where
    go :: Ast Name a -> SetMap
    go e = 
      case e of
        Global (ADef name typ) -> singleton typ True name
        Global (SDef _ name typ _) -> singleton typ False name
        _ -> composeFold go e


generateGlobalSetters :: Map (Type, Bool) (Set Name) -> [Ast Name Toplevel]
generateGlobalSetters = map mkFn
                      . map (second $ (`zip` [1..]) . Set.toList) 
                      . Map.toList 
  where
    mkFn :: ((Type, Bool), [(Name, Int)]) -> Ast Name Toplevel
    mkFn ((typ, isarray), names) = _



bin :: Int -> Int -> Ast Name Expr -> (Int -> Ast Name Stmt) -> Ast Name Stmt
bin lo hi c f = go lo (hi+1)
  where
    go lo hi
        | lo +1== hi = f lo
        | otherwise =
            let mid = (lo+hi) `div` 2
                thenB = go lo mid
                elseB = go mid hi
            in If (Call "<" [c, Int . fromString $ show mid]) [thenB] [] (Just [elseB])

