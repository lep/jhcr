{-# LANGUAGE GADTs #-}

module Jass.LCA (child2parent, lca) where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Composeable
import Data.List (unfoldr)
import Data.Hashable

import qualified Data.LCA.Online as LCA

import Jass.Ast

child2parent :: Ast v x -> Map Type Type
child2parent x = Map.singleton "integer" "real" <> go x
  where
    go :: Ast v x -> Map Type Type
    go x =
      case x of
          Typedef a b -> Map.singleton a b
          _ -> composeFold child2parent x

path' :: Type -> Map Type Type -> [Type]
path' k0 m = k0:unfoldr (\k -> Map.lookup k m >>= \x -> pure(x,x)) k0

path :: Type -> Map Type Type -> LCA.Path Type
path k0 m = LCA.fromList . map (\k -> (hash k, k)) $ path' k0 m

lca :: Map Type Type -> Type -> Type -> Maybe Type
lca m a b = do
    let p1 = path a m
        p2 = path b m
    (_, v, _) <- LCA.uncons $ LCA.lca p1 p2
    return v
