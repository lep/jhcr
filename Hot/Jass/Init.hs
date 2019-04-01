{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}


module Hot.Jass.Init where

import Control.Lens hiding (Const)

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader


import Data.Int
import Data.Monoid
import Data.List
import Data.String
import Data.Function
import Data.Maybe

import GHC.Generics
import Data.Binary

import Data.DList (DList)
import qualified Data.DList as DList

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


import Jass.Ast hiding (fmap, foldMap, traverse)

import Data.Composeable

import Hot.Var (Var, mkFn, mkGlobal, mkLocal, mkOp, getId, getId', nameOf, (##))
import qualified Hot.Var as H
import qualified Hot.Types as Hot

import Unsafe.Coerce
import Debug.Trace


init_name2ids :: Ast Var Programm -> Ast Var Programm
init_name2ids x =
    Programm
        [ Function Normal (mkFn "_Auto_name_tables_init") [] "nothing" $
            DList.toList $ go x
        ]
  where
    go :: Ast Var x -> DList (Ast Var Stmt)
    go x =
      case x of
        Global (SDef _ v _ _) -> DList.singleton $ addInit v
        Function _ v _ _ _ -> DList.singleton $ addInit v
        Native _ v _ _ -> DList.singleton $ addInit v
        _ -> composeFold go x
    addInit v =
      case v of
        -- call _Names_insert_global(ty, name, uid)
        H.Global _ name ty isarray uid ->
            Call (mkFn "_Names_insert_global")
                 [ Int . fromString . show $ (Map.findWithDefault (error "Unknown type") ty Hot.types)
                 , String name
                 , Int . fromString $ show uid
                 ]

        -- call _Names_insert_function(name, uid)
        H.Fn name _ _ uid ->
            Call (mkFn "_Names_insert_function")
                 [ String name
                 , Int . fromString $ show uid
                 ]



    





