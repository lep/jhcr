{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Hot.JassHelper (compile) where

import Data.List

import Jass.Ast hiding (fmap, traverse)

import Data.Composeable


cleanName :: Name -> Name
cleanName x =
  let x' = concatMap (\case '_':'_':_ -> "__"; x -> x) $ group x
  in if "jasshelper__init" `isPrefixOf` x
  then "jasshelper__init0000"
  else x'


-- TODO: Maybe operate on Hot.Ast to only rename globals
compile :: Ast Name x -> Ast Name x
compile x =
  case x of
    Code f -> Code $ cleanName f
    Call "ExecuteFunc" [String f] -> Call "ExecuteFunc" [String $ cleanName f]
    Call f args -> Call (cleanName f) $ map compile args
    ADef n ty -> ADef (cleanName n) ty
    SDef c n ty init -> SDef c (cleanName n) ty $ fmap compile init
    SVar n -> SVar $ cleanName n
    AVar n idx -> AVar (cleanName n) $ compile idx
    Function c name args ret body ->
        Function c (cleanName name) args ret $ map compile body
   
    _ -> composeOp compile x
